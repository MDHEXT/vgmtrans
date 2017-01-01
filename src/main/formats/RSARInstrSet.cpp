#include "pch.h"
#include "RSARInstrSet.h"

static const double INTR_FREQUENCY = 1.0 / 192.0;

static uint16_t GetFallingRate(uint8_t DecayTime) {
  uint32_t realDecay;
  if (DecayTime == 0x7F)
    realDecay = 0xFFFF;
  else if (DecayTime == 0x7E)
    realDecay = 0x3C00;
  else if (DecayTime < 0x32) {
    realDecay = DecayTime * 2;
    ++realDecay;
    realDecay &= 0xFFFF;
  }
  else {
    realDecay = 0x1E00;
    DecayTime = 0x7E - DecayTime;
    realDecay /= DecayTime;     //there is a whole subroutine that seems to resolve simply to this.  I have tested all cases
    realDecay &= 0xFFFF;
  }
  return (uint16_t)realDecay;
}

/* Stolen from NDSInstrSet.cpp since the format is similar. */
static void SetupEnvelope(VGMRgn *rgn, uint8_t AttackTime, uint8_t ReleaseTime, uint8_t SustainLev, uint8_t DecayTime) {
  short realDecay;
  short realRelease;
  uint8_t realAttack;
  long realSustainLev;
  const uint8_t AttackTimeTable[] = { 0x00, 0x01, 0x05, 0x0E, 0x1A, 0x26,
                                      0x33, 0x3F, 0x49, 0x54, 0x5C, 0x64, 0x6D, 0x74, 0x7B, 0x7F, 0x84,
                                      0x89, 0x8F };

  const uint16_t sustainLevTable[] =
    { 0xFD2D, 0xFD2E, 0xFD2F, 0xFD75, 0xFDA7, 0xFDCE, 0xFDEE, 0xFE09, 0xFE20, 0xFE34, 0xFE46, 0xFE57, 0xFE66, 0xFE74,
      0xFE81, 0xFE8D, 0xFE98, 0xFEA3, 0xFEAD, 0xFEB6, 0xFEBF, 0xFEC7, 0xFECF, 0xFED7, 0xFEDF, 0xFEE6, 0xFEEC, 0xFEF3,
      0xFEF9, 0xFEFF, 0xFF05, 0xFF0B, 0xFF11, 0xFF16, 0xFF1B, 0xFF20, 0xFF25, 0xFF2A, 0xFF2E, 0xFF33, 0xFF37, 0xFF3C,
      0xFF40, 0xFF44, 0xFF48, 0xFF4C, 0xFF50, 0xFF53, 0xFF57, 0xFF5B, 0xFF5E, 0xFF62, 0xFF65, 0xFF68, 0xFF6B, 0xFF6F,
      0xFF72, 0xFF75, 0xFF78, 0xFF7B, 0xFF7E, 0xFF81, 0xFF83, 0xFF86, 0xFF89, 0xFF8C, 0xFF8E, 0xFF91, 0xFF93, 0xFF96,
      0xFF99, 0xFF9B, 0xFF9D, 0xFFA0, 0xFFA2, 0xFFA5, 0xFFA7, 0xFFA9, 0xFFAB, 0xFFAE, 0xFFB0, 0xFFB2, 0xFFB4, 0xFFB6,
      0xFFB8, 0xFFBA, 0xFFBC, 0xFFBE, 0xFFC0, 0xFFC2, 0xFFC4, 0xFFC6, 0xFFC8, 0xFFCA, 0xFFCC, 0xFFCE, 0xFFCF, 0xFFD1,
      0xFFD3, 0xFFD5, 0xFFD6, 0xFFD8, 0xFFDA, 0xFFDC, 0xFFDD, 0xFFDF, 0xFFE1, 0xFFE2, 0xFFE4, 0xFFE5, 0xFFE7, 0xFFE9,
      0xFFEA, 0xFFEC, 0xFFED, 0xFFEF, 0xFFF0, 0xFFF2, 0xFFF3, 0xFFF5, 0xFFF6, 0xFFF8, 0xFFF9, 0xFFFA, 0xFFFC, 0xFFFD,
      0xFFFF, 0x0000 };

  if (AttackTime >= 0x6D)
    realAttack = AttackTimeTable[0x7F - AttackTime];
  else
    realAttack = 0xFF - AttackTime;

  realDecay = GetFallingRate(DecayTime);
  realRelease = GetFallingRate(ReleaseTime);

  int count = 0;
  for (long i = 0x16980; i != 0; i = (i * realAttack) >> 8)
    count++;
  rgn->attack_time = count * INTR_FREQUENCY;

  if (SustainLev == 0x7F)
    realSustainLev = 0;
  else
    realSustainLev = (0x10000 - sustainLevTable[SustainLev]) << 7;
  if (DecayTime == 0x7F)
    rgn->decay_time = 0.001;
  else {
    count = 0x16980 / realDecay;//realSustainLev / realDecay;
    rgn->decay_time = count * INTR_FREQUENCY;
  }

  if (realSustainLev == 0)
    rgn->sustain_level = 1.0;
  else
    //rgn->sustain_level = 20 * log10 ((92544.0-realSustainLev) / 92544.0);
    rgn->sustain_level = (double)(0x16980 - realSustainLev) / (double)0x16980;

  //we express release rate as time from maximum volume, not sustain level
  count = 0x16980 / realRelease;
  rgn->release_time = count * INTR_FREQUENCY;
}

bool RBNKInstr::LoadInstr() {
  std::vector<Region> keyRegions = EnumerateRegionTable(dwOffset);

  for (uint32_t i = 0; i < keyRegions.size(); i++) {
    Region keyRegion = keyRegions[i];
    uint8_t keyLow = keyRegion.low;
    uint8_t keyHigh = (i + 1 < keyRegions.size()) ? keyRegions[i + 1].low : 0x7F;

    std::vector<Region> velRegions = EnumerateRegionTable(keyRegion.subRefOffs);
    for (uint32_t j = 0; j < velRegions.size(); j++) {
      Region velRegion = velRegions[j];
      uint8_t velLow = velRegion.low;
      uint8_t velHigh = (j + 1 < velRegions.size()) ? velRegions[j + 1].low : 0x7F;

      uint32_t offs = parInstrSet->dwOffset + GetWordBE(velRegion.subRefOffs + 0x04);
      VGMRgn *rgn = AddRgn(offs, 0, 0, keyLow, keyHigh, velLow, velHigh);
      LoadRgn(rgn);
    }
  }

  return true;
}

std::vector<RBNKInstr::Region> RBNKInstr::EnumerateRegionTable(uint32_t refOffset) {
  RegionSet regionSet = (RegionSet)GetByte(refOffset + 0x01);

  /* First, enumerate based on key. */
  switch (regionSet) {
  case RegionSet::DIRECT:
    return{ { 0, refOffset } };
  case RegionSet::RANGE: {
    uint32_t offs = parInstrSet->dwOffset + GetWordBE(refOffset + 0x04);
    uint8_t tableSize = GetByte(offs);
    uint32_t tableEnd = (offs + 1 + tableSize + 3) & ~3;

    std::vector<Region> table;
    for (uint8_t i = 0; i < tableSize; i++) {
      uint32_t low = GetByte(offs + 1 + i);
      uint32_t subRefOffs = tableEnd + (i * 8);
      Region region = { low, subRefOffs };
      table.push_back(region);
    }
    return table;
  }
  case RegionSet::INDEX:
    /* TODO: Implement INDEX region sets. */
    return{};
  case RegionSet::NONE:
    return{};
  default:
    assert(false && "Unsupported region");
    return{};
  }
}

void RBNKInstr::LoadRgn(VGMRgn *rgn) {
  uint32_t rgnBase = rgn->dwOffset;
  uint32_t waveIndex = GetWordBE(rgnBase + 0x00);
  rgn->SetSampNum(waveIndex);

  uint8_t a = GetByte(rgnBase + 0x04);
  uint8_t d = GetByte(rgnBase + 0x05);
  uint8_t s = GetByte(rgnBase + 0x06);
  uint8_t r = GetByte(rgnBase + 0x07);

  /* XXX: This doesn't seem to work. Figure this out later. */
  /* SetupEnvelope(rgn, a, d, s, r); */

  /* hold */
  uint8_t waveDataLocationType = GetByte(rgnBase + 0x09);
  assert(waveDataLocationType == 0x00 && "We only support INDEX wave data for now.");

  /* noteOffType */
  /* alternateAssign */
  rgn->SetUnityKey(GetByte(rgnBase + 0x0C));

  /* XXX: How do I transcribe volume? */
  /* rgn->SetVolume(GetByte(rgnBase + 0x0D)); */

  rgn->SetPan(GetByte(rgnBase + 0x0E));
  /* padding */
  /* f32 tune */
  /* lfo table */
  /* graph env table */
  /* randomizer table */
}

bool RSARInstrSet::GetInstrPointers() {
  uint32_t instTableCount = GetWordBE(dwOffset + 0x00);
  uint32_t instTableIdx = dwOffset + 0x04;
  for (uint32_t i = 0; i < instTableCount; i++) {
    /* Some RBNK files have NULL offsets here. Don't crash in that case. */
    uint32_t instKind = GetByte(instTableIdx + 0x01);
    if (instKind != 0x00)
      aInstrs.push_back(new RBNKInstr(this, instTableIdx, 0, i));
    instTableIdx += 0x08;
  }
  return true;
}

uint32_t RBNKSamp::DspAddressToSamples(uint32_t dspAddr) {
  switch (format) {
  case Format::ADPCM:
    return (dspAddr / 16) * 14 + (dspAddr % 16) - 2;
  case Format::PCM16:
  case Format::PCM8:
  default:
    return dspAddr;
  }
}

void RBNKSamp::ReadAdpcmParam(uint32_t adpcmParamBase, AdpcmParam *param) {
  for (int i = 0; i < 16; i++)
    param->coef[i] = GetShortBE(adpcmParamBase + i * 0x02);
  param->gain = GetShortBE(adpcmParamBase + 0x20);
  param->expectedScale = GetShortBE(adpcmParamBase + 0x22);
  param->yn1 = GetShortBE(adpcmParamBase + 0x24);
  param->yn2 = GetShortBE(adpcmParamBase + 0x26);

  /* Make sure that these are sane. */
  assert(param->gain == 0x00);
  assert(param->yn2 == 0x00);
  assert(param->yn1 == 0x00);
}

void RBNKSamp::Load() {
  format = (Format)GetByte(dwOffset + 0x00);

  switch (format) {
  case Format::PCM8:
    SetWaveType(WT_PCM8);
    SetBPS(8);
    break;
  case Format::PCM16:
  case Format::ADPCM:
    SetWaveType(WT_PCM16);
    SetBPS(16);
  }

  bool loop = !!GetByte(dwOffset + 0x01);

  SetNumChannels(GetByte(dwOffset + 0x02));
  assert((channels <= MAX_CHANNELS) && "We only support up to two channels.");

  uint8_t sampleRateH = GetByte(dwOffset + 0x03);
  uint16_t sampleRateL = GetShortBE(dwOffset + 0x04);
  SetRate((sampleRateH << 16) | sampleRateL);

  uint8_t dataLocationType = GetByte(dwOffset + 0x06);
  assert((dataLocationType == 0x00) && "We only support OFFSET-based wave data locations.");
  /* padding */

  uint32_t loopStart = GetWordBE(dwOffset + 0x08);
  uint32_t loopEnd = GetWordBE(dwOffset + 0x0C);

  /*
    static int nsmp = 0;
    char debug[255];
    sprintf(debug, "%d %d %d %d %d\n", nsmp++, DspAddressToSamples(loopStart), DspAddressToSamples(loopEnd),
    DspAddressToSamples(loopEnd) - DspAddressToSamples(loopStart),
    DspAddressToSamples(loopEnd - loopStart));
    OutputDebugStringA(debug);
  */

  SetLoopStatus(loop);
  if (loop) {
    SetLoopStartMeasure(LM_SAMPLES);
    SetLoopLengthMeasure(LM_SAMPLES);
    /* XXX: Not sure about these exactly but it makes Wii Shop sound correct.
     * Probably specific to PCM16 though... */
    SetLoopOffset(DspAddressToSamples(loopStart) - 2);
    SetLoopLength(DspAddressToSamples(loopEnd) - DspAddressToSamples(loopStart) + 2);
  }

  uint32_t channelInfoTableBase = dwOffset + GetWordBE(dwOffset + 0x10);
  for (int i = 0; i < channels; i++) {
    ChannelParam *channelParam = &channelParams[i];

    uint32_t channelInfoBase = dwOffset + GetWordBE(channelInfoTableBase + (i * 4));
    channelParam->dataOffset = GetWordBE(channelInfoBase + 0x00);

    uint32_t adpcmOffset = GetWordBE(channelInfoBase + 0x04);
    /* volumeFrontL, volumeFrontR, volumeRearL, volumeRearR */

    if (adpcmOffset != 0) {
      uint32_t adpcmBase = dwOffset + adpcmOffset;
      ReadAdpcmParam(adpcmBase, &channelParam->adpcmParam);
    }
  }

  uint32_t dataLocation = GetWordBE(dwOffset + 0x14);
  dataOff += dataLocation;

  /* Quite sure this maps to end bytes, even without looping. */
  dataLength = loopEnd;
  ulUncompressedSize = DspAddressToSamples(loopEnd) * (bps / 8) * channels;

  name = (format == PCM8 ? L"PCM8" : format == PCM16 ? L"PCM16" : L"ADPCM");
}

static int16_t Clamp16(int32_t sample) {
  if (sample > 0x7FFF) return  0x7FFF;
  if (sample < -0x8000) return -0x8000;
  return sample;
}

void RBNKSamp::ConvertAdpcmChannel(ChannelParam *param, uint8_t *buf, int channelSpacing) {
  int16_t hist1 = param->adpcmParam.yn1, hist2 = param->adpcmParam.yn2;

  uint32_t samplesDone = 0;
  uint32_t numSamples = DspAddressToSamples(dataLength);

  /* Go over and convert each frame of 14 samples. */
  uint32_t srcOffs = dataOff + param->dataOffset;
  uint32_t dstOffs = 0;
  while (samplesDone < numSamples) {
    /* Each frame is 8 bytes -- a one-byte header, and then seven bytes
     * of samples. Each sample is 4 bits, so we have 14 samples per frame. */
    uint8_t frameHeader = GetByte(srcOffs++);

    /* Sanity check. */
    if (samplesDone == 0)
      assert(frameHeader == param->adpcmParam.expectedScale);

    int32_t scale = 1 << (frameHeader & 0x0F);
    uint8_t coefIdx = (frameHeader >> 4);
    int16_t coef1 = (int16_t)param->adpcmParam.coef[coefIdx * 2], coef2 = (int16_t)param->adpcmParam.coef[coefIdx * 2 + 1];

    for (int i = 0; i < 14; i++) {
      /* Pull out the correct byte and nibble. */
      uint8_t byte = GetByte(srcOffs + (i >> 1));
      uint8_t unsignedNibble = (i & 1) ? (byte & 0x0F) : (byte >> 4);

      /* Sign extend. */
      int8_t signedNibble = ((int8_t)(unsignedNibble << 4)) >> 4;

      /* Decode. */
      int16_t sample = Clamp16((((signedNibble * scale) << 11) + 1024 + (coef1*hist1 + coef2*hist2)) >> 11);
      ((int16_t *)buf)[dstOffs] = sample;
      dstOffs += channelSpacing;

      hist2 = hist1;
      hist1 = sample;

      samplesDone++;
      if (samplesDone >= numSamples)
        break;
    }

    srcOffs += 7;
  }
}

void RBNKSamp::ConvertPCM8Channel(ChannelParam *param, uint8_t *buf, int channelSpacing) {
  uint32_t srcOffs = dataOff + param->dataOffset;
  uint32_t numSamples = DspAddressToSamples(dataLength);
  GetBytes(srcOffs, numSamples, buf);
}

void RBNKSamp::ConvertPCM16Channel(ChannelParam *param, uint8_t *buf, int channelSpacing) {
  /* Do this the long way because of endianness issues... */

  uint32_t srcOffs = dataOff + param->dataOffset;
  uint32_t dstOffs = 0;

  uint32_t samplesDone = 0;
  uint32_t numSamples = DspAddressToSamples(dataLength);

  while (samplesDone < numSamples) {
    uint16_t sample = GetWordBE(srcOffs);
    srcOffs += 2;

    ((int16_t *)buf)[dstOffs] = sample;
    dstOffs += channelSpacing;

    samplesDone++;
  }
}

void RBNKSamp::ConvertChannel(ChannelParam *param, uint8_t *buf, int channelSpacing) {
  if (format == ADPCM)
    ConvertAdpcmChannel(param, buf, channelSpacing);
  else if (format == PCM8)
    ConvertPCM8Channel(param, buf, channelSpacing);
  else if (format == PCM16)
    ConvertPCM16Channel(param, buf, channelSpacing);
}

void RBNKSamp::ConvertToStdWave(uint8_t *buf) {
  int bytesPerSample = bps / 8;
  for (int i = 0; i < channels; i++)
    ConvertChannel(&channelParams[i], &buf[i*bytesPerSample], channels);
}

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

bool RSARSampCollWAVE::GetSampleInfo() {
  uint32_t waveTableCount = GetWordBE(dwOffset + 0x00);
  uint32_t waveTableIdx = dwOffset + 0x08;
  for (uint32_t i = 0; i < waveTableCount; i++) {
    uint32_t waveOffs = dwOffset + GetWordBE(waveTableIdx);
    samples.push_back(new RBNKSamp(this, waveOffs, 0, waveDataOffset, 0));
    waveTableIdx += 0x08;
  }
  return true;
}

VGMSamp * RSARSampCollRWAR::ParseRWAVFile(uint32_t offset) {
  if (!MatchMagic(rawfile, offset, "RWAV\xFE\xFF\x01"))
    return nullptr;

  uint32_t infoBlockOffs = offset + GetWordBE(offset + 0x10);
  uint32_t dataBlockOffs = offset + GetWordBE(offset + 0x18);
  FileRange infoBlock = CheckBlock(rawfile, infoBlockOffs, "INFO");
  FileRange dataBlock = CheckBlock(rawfile, dataBlockOffs, "DATA");

  /* Bizarrely enough, dataLocation inside the WAVE info points to the start
   * of the DATA block header so add an extra 0x08. */
  uint32_t dataOffset = infoBlock.offset + 0x08;
  uint32_t dataSize = dataBlock.size + 0x08;

  return new RBNKSamp(this, infoBlock.offset, infoBlock.size, dataOffset, dataSize);
}

bool RSARSampCollRWAR::GetHeaderInfo() {
  AddHeader(dwOffset, 8, L"RWAR Signature");
  if (!MatchMagic(rawfile, dwOffset, "RWAR\xFE\xFF\x01"))
    return false;

  uint32_t tablBlockOffs = dwOffset + GetWordBE(dwOffset + 0x10);
  uint32_t dataBlockOffs = dwOffset + GetWordBE(dwOffset + 0x18);
  FileRange tablBlock = CheckBlock(rawfile, tablBlockOffs, "TABL");

  uint32_t waveTableCount = GetWordBE(tablBlock.offset + 0x00);
  uint32_t waveTableIdx = tablBlock.offset + 0x08;
  for (uint32_t i = 0; i < waveTableCount; i++) {
    uint32_t rwavHeaderOffs = dataBlockOffs + GetWordBE(waveTableIdx);
    uint32_t rwavHeaderSize = GetWordBE(waveTableIdx + 0x04);
    samples.push_back(ParseRWAVFile(rwavHeaderOffs));
    waveTableIdx += 0x0C;
  }

  return true;
}

bool RSARSampCollRWAR::GetSampleInfo() {
  return true;
}

RBNK RBNK::Parse(RawFile *file, std::wstring name, uint32_t rbnkOffset, uint32_t waveDataOffset) {
  if (!MatchMagic(file, rbnkOffset, "RBNK\xFE\xFF\x01"))
    return { nullptr, nullptr };

  uint32_t version = file->GetByte(rbnkOffset + 0x07);
  uint32_t dataBlockOffs = rbnkOffset + file->GetWordBE(rbnkOffset + 0x10);
  uint32_t waveBlockOffs = rbnkOffset + file->GetWordBE(rbnkOffset + 0x18);

  bool success;
  FileRange dataBlock = CheckBlock(file, dataBlockOffs, "DATA");
  VGMInstrSet *instrSet = new RSARInstrSet(file, dataBlock.offset, dataBlock.size, name);
  success = instrSet->LoadVGMFile();
  // assert(success);

  FileRange waveBlock = CheckBlock(file, waveBlockOffs, "WAVE");
  VGMSampColl *sampColl;
  if (waveBlock.size != 0)
    sampColl = new RSARSampCollWAVE(file, waveBlock.offset, waveBlock.size, waveDataOffset, 0);
  else
    sampColl = new RSARSampCollRWAR(file, waveDataOffset, 0);
  success = sampColl->LoadVGMFile();
  // assert(success);

  RBNK bank = { instrSet, sampColl };
  return bank;
}
