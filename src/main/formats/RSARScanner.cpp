#include "pch.h"

#include "RSARFormat.h"
#include "RSARScanner.h"
#include "RSARSeq.h"
#include "RSARInstrSet.h"

DECLARE_FORMAT(RSAR);

/* Utility */

struct FileRange {
  uint32_t offset;
  uint32_t size;
};

static bool MatchMagic(RawFile *file, uint32_t offs, const char *magic) {
  char buf[16];
  assert(sizeof(buf) >= strlen(magic));
  file->GetBytes(offs, strlen(magic), buf);
  return memcmp(buf, magic, strlen(magic)) == 0;
}

static FileRange CheckBlock(RawFile *file, uint32_t offs, const char *magic) {
  if (!MatchMagic(file, offs, magic))
    return{ 0, 0 };

  uint32_t size = file->GetWordBE(offs + 0x04);
  return{ offs + 0x08, size - 0x08 };
}

/* RBNK */

/* RSAR */

struct Sound {
  std::string name;
  uint32_t fileID;

  enum Type { SEQ = 1, STRM = 2, WAVE = 3 } type;
  struct {
    uint32_t dataOffset;
    uint32_t bankID;
    uint32_t allocTrack;
  } seq;
};

struct Bank {
  std::string name;
  uint32_t fileID;
};

struct GroupItem {
  uint32_t fileID;
  FileRange data;
  FileRange waveData;
};

struct Group {
  std::string name;
  FileRange data;
  FileRange waveData;
  std::vector<GroupItem> items;
};

struct File {
  uint32_t groupID;
  uint32_t index;
};

struct Context {
  RawFile *file;

  /* RSAR structure internals... */
  std::vector<std::string> stringTable;
  std::vector<Sound> soundTable;
  std::vector<Bank> bankTable;
  std::vector<File> fileTable;
  std::vector<Group> groupTable;

  /* Parsed stuff. */
  std::vector<RBNK> rbnks;
};

static std::vector<std::string> ParseSymbBlock(RawFile *file, uint32_t blockBase) {
  uint32_t strTableBase = blockBase + file->GetWordBE(blockBase);
  uint32_t strTableCount = file->GetWordBE(strTableBase + 0x00);

  std::vector<std::string> table(strTableCount);
  uint32_t strTableIdx = strTableBase + 0x04;
  for (uint32_t i = 0; i < strTableCount; i++) {
    uint32_t strOffs = file->GetWordBE(strTableIdx);
    strTableIdx += 0x04;

    uint8_t *strP = &file->buf.data[blockBase + strOffs];
    std::string str((char *)strP);
    table[i] = str;
  }

  return table;
}

static std::vector<Sound> ReadSoundTable(Context *context, RawFile *file, uint32_t infoBlockOffs) {
  uint32_t soundTableOffs = file->GetWordBE(infoBlockOffs + 0x04);
  uint32_t soundTableCount = file->GetWordBE(infoBlockOffs + soundTableOffs);
  std::vector<Sound> soundTable;

  uint32_t soundTableIdx = infoBlockOffs + soundTableOffs + 0x08;
  for (uint32_t i = 0; i < soundTableCount; i++) {
    uint32_t soundBase = infoBlockOffs + file->GetWordBE(soundTableIdx);
    soundTableIdx += 0x08;

    Sound sound;
    uint32_t stringID = file->GetWordBE(soundBase + 0x00);
    if (stringID != 0xFFFFFFFF)
      sound.name = context->stringTable[stringID];
    sound.fileID = file->GetWordBE(soundBase + 0x04);
    /* 0x08 player */
    /* 0x0C param3DRef */
    /* 0x14 volume */
    /* 0x15 playerPriority */
    sound.type = (Sound::Type) file->GetByte(soundBase + 0x16);
    if (sound.type != Sound::Type::SEQ)
      continue;

    /* 0x17 remoteFilter */
    uint32_t seqInfoOffs = infoBlockOffs + file->GetWordBE(soundBase + 0x1C);

    sound.seq.dataOffset = file->GetWordBE(seqInfoOffs + 0x00);
    sound.seq.bankID = file->GetWordBE(seqInfoOffs + 0x04);
    sound.seq.allocTrack = file->GetWordBE(seqInfoOffs + 0x08);
    soundTable.push_back(sound);
  }

  return soundTable;
}

static std::vector<Bank> ReadBankTable(Context *context, RawFile *file, uint32_t infoBlockOffs) {
  uint32_t bankTableOffs = file->GetWordBE(infoBlockOffs + 0x0C);
  uint32_t bankTableCount = file->GetWordBE(infoBlockOffs + bankTableOffs);
  std::vector<Bank> bankTable;

  uint32_t bankTableIdx = infoBlockOffs + bankTableOffs + 0x08;
  for (uint32_t i = 0; i < bankTableCount; i++) {
    uint32_t bankBase = infoBlockOffs + file->GetWordBE(bankTableIdx);
    bankTableIdx += 0x08;

    Bank bank;
    uint32_t stringID = file->GetWordBE(bankBase + 0x00);
    if (stringID != 0xFFFFFFFF)
      bank.name = context->stringTable[stringID];
    bank.fileID = file->GetWordBE(bankBase + 0x04);
    bankTable.push_back(bank);
  }

  return bankTable;
}

static std::vector<File> ReadFileTable(Context *context, RawFile *file, uint32_t infoBlockOffs) {
  uint32_t fileTableOffs = file->GetWordBE(infoBlockOffs + 0x1C);
  uint32_t fileTableCount = file->GetWordBE(infoBlockOffs + fileTableOffs);
  std::vector<File> fileTable;

  uint32_t fileTableIdx = infoBlockOffs + fileTableOffs + 0x08;
  for (uint32_t i = 0; i < fileTableCount; i++) {
    uint32_t fileBase = infoBlockOffs + file->GetWordBE(fileTableIdx);
    fileTableIdx += 0x08;

    File rsarFile = {};
    uint32_t filePosTableBase = infoBlockOffs + file->GetWordBE(fileBase + 0x18);
    uint32_t filePosTableCount = file->GetWordBE(filePosTableBase);

    /* Push a dummy file if we don't have anything and hope nothing references it... */
    if (filePosTableCount == 0) {
      fileTable.push_back(rsarFile);
      continue;
    }

    uint32_t filePosBase = infoBlockOffs + file->GetWordBE(filePosTableBase + 0x08);
    rsarFile.groupID = file->GetWordBE(filePosBase + 0x00);
    rsarFile.index = file->GetWordBE(filePosBase + 0x04);
    fileTable.push_back(rsarFile);
  }

  return fileTable;
}

static std::vector<Group> ReadGroupTable(Context *context, RawFile *file, uint32_t infoBlockOffs) {
  uint32_t groupTableOffs = file->GetWordBE(infoBlockOffs + 0x24);
  uint32_t groupTableCount = file->GetWordBE(infoBlockOffs + groupTableOffs);
  std::vector<Group> groupTable;

  uint32_t groupTableIdx = infoBlockOffs + groupTableOffs + 0x08;
  for (uint32_t i = 0; i < groupTableCount; i++) {
    uint32_t groupBase = infoBlockOffs + file->GetWordBE(groupTableIdx);
    groupTableIdx += 0x08;

    uint32_t stringID = file->GetWordBE(groupBase + 0x00);
    Group group;
    if (stringID != 0xFFFFFFFF)
      group.name = context->stringTable[stringID];
    group.data.offset = file->GetWordBE(groupBase + 0x10);
    group.data.size = file->GetWordBE(groupBase + 0x14);
    group.waveData.offset = file->GetWordBE(groupBase + 0x18);
    group.waveData.size = file->GetWordBE(groupBase + 0x1C);

    uint32_t itemTableBase = infoBlockOffs + file->GetWordBE(groupBase + 0x24);
    uint32_t itemTableCount = file->GetWordBE(itemTableBase);
    uint32_t itemTableIdx = itemTableBase + 0x08;
    for (uint32_t j = 0; j < itemTableCount; j++) {
      uint32_t itemBase = infoBlockOffs + file->GetWordBE(itemTableIdx);
      itemTableIdx += 0x08;

      GroupItem item;
      item.fileID = file->GetWordBE(itemBase + 0x00);
      item.data.offset = file->GetWordBE(itemBase + 0x04);
      item.data.size = file->GetWordBE(itemBase + 0x08);
      item.waveData.offset = file->GetWordBE(itemBase + 0x0C);
      item.waveData.size = file->GetWordBE(itemBase + 0x10);
      group.items.push_back(item);
    }

    groupTable.push_back(group);
  }

  return groupTable;
}

static RBNK LoadBank(Context *context, Bank *bank) {
  File fileInfo = context->fileTable[bank->fileID];
  Group groupInfo = context->groupTable[fileInfo.groupID];
  GroupItem itemInfo = groupInfo.items[fileInfo.index];

  assert(itemInfo.fileID == bank->fileID);
  uint32_t rbnkOffset = itemInfo.data.offset + groupInfo.data.offset;
  uint32_t waveOffset = itemInfo.waveData.offset + groupInfo.waveData.offset;

  return RBNK::Parse(context->file, string2wstring(bank->name), rbnkOffset, waveOffset);
}

static void LoadSeq(Context *context, Sound *sound) {
  assert(sound->type == Sound::SEQ);

  File fileInfo = context->fileTable[sound->fileID];
  Group groupInfo = context->groupTable[fileInfo.groupID];
  GroupItem itemInfo = groupInfo.items[fileInfo.index];

  bool success;

  uint32_t rseqOffset = itemInfo.data.offset + groupInfo.data.offset;
  VGMSeq *seq = RSARSeq::Parse(context->file, string2wstring(sound->name), rseqOffset, sound->seq.dataOffset);
  success = seq->LoadVGMFile();
  assert(success);
  RBNK rbnk = context->rbnks[sound->seq.bankID];

  VGMColl *coll = new VGMColl(string2wstring(sound->name));
  if (rbnk.instrSet)
    coll->AddInstrSet(rbnk.instrSet);
  if (rbnk.sampColl)
    coll->AddSampColl(rbnk.sampColl);
  coll->UseSeq(seq);
  success = coll->Load();
  assert(success);
}

void RSARScanner::Scan(RawFile *file, void *info) {
  if (!MatchMagic(file, 0, "RSAR\xFE\xFF\x01"))
    return;

  uint32_t version = file->GetByte(0x07);
  uint32_t symbBlockOffs = file->GetWordBE(0x10);
  uint32_t infoBlockOffs = file->GetWordBE(0x18);
  uint32_t fileBlockOffs = file->GetWordBE(0x20);

  FileRange symbBlockBase = CheckBlock(file, symbBlockOffs, "SYMB");
  std::vector<std::string> strTable = ParseSymbBlock(file, symbBlockBase.offset);

  Context context;
  context.file = file;
  context.stringTable = strTable;

  FileRange infoBlockBase = CheckBlock(file, infoBlockOffs, "INFO");

  /*
  uint32_t playerTableOffs = file->GetWordBE(infoBlockBase + 0x14);
  */

  context.soundTable = ReadSoundTable(&context, file, infoBlockBase.offset);
  context.bankTable = ReadBankTable(&context, file, infoBlockBase.offset);
  context.fileTable = ReadFileTable(&context, file, infoBlockBase.offset);
  context.groupTable = ReadGroupTable(&context, file, infoBlockBase.offset);

  /* Load all BNKS. */
  for (size_t i = 0; i < context.bankTable.size(); i++)
    context.rbnks.push_back(LoadBank(&context, &context.bankTable[i]));

  for (size_t i = 0; i < context.soundTable.size(); i++)
    LoadSeq(&context, &context.soundTable[i]);
}
