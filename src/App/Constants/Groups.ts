import { t as _IC } from "../Utilities/IC.gen"

export enum ProfessionGroup {
  Mundane = 1,
  Magical = 2,
  Blessed = 3,
}

export enum ProfessionSubGroup {
  NonFighters = 1,
  Fighters = 2,
  GuildMages = 1,
  Witches = 2,
  Elves = 3,
  Druids = 4,
  Scharlatane = 5,
  Zauberbarden = 6,
  Zaubertänzer = 7,
  IntuitiveZauberer = 8,
  Meistertalentierte = 9,
  Qabalyamagier = 10,
  Kristallomanten = 11,
  Geoden = 12,
  Alchimisten = 13,
  Schelme = 14,
  BlessedOnesOfTheTwelveGods = 1,
}

export enum IC {
  A = 1,
  B,
  C,
  D,
  E,
}

export const icFromJs = (x: IC): _IC => {
  switch (x) {
    case IC.A: return "A"
    case IC.B: return "B"
    case IC.C: return "C"
    case IC.D: return "D"
    case IC.E: return "E"
    default: throw new TypeError (`icFromJs: ${x} is not an IC`)
  }
}

export const icToJs = (x: _IC): IC => {
  switch (x) {
    case "A": return IC.A
    case "B": return IC.B
    case "C": return IC.C
    case "D": return IC.D
    case "E": return IC.E
    default: throw new TypeError (`icToJs: ${x} is not an IC`)
  }
}

export enum SkillGroup {
  Physical = 1,
  Social = 2,
  Nature = 3,
  Knowledge = 4,
  Craft = 5,
  Think=6,
}

export enum CombatTechniqueGroupId {
  Melee = 1,
  Ranged = 2,
}

export enum SpecialAbilityGroup {
  General = 1,
  Fate = 2,
  Combat = 3,
  Magical = 4,
  StaffEnchantments = 5,
  Witch = 6,
  Karma = 7,
  ProtectiveWardingCircles = 8,
  CombatStylesArmed = 9,
  CombatStylesUnarmed = 10,
  CombatExtended = 11,
  Commands = 12,
  MagicalStyles = 13,
  MagicalExtended = 14,
  Bannschwert = 15,
  Dolch = 16,
  Instrument = 17,
  Gewand = 18,
  Kugel = 19,
  Stecken = 20,
  Prügel = 21,
  Ahnenzeichen = 22,
  Zeremonialgegenstände = 23,
  Predigten = 24,
  BlessedStyles = 25,
  KarmaExtended = 26,
  Visionen = 27,
  MagicalTraditions = 28,
  BlessedTraditions = 29,
  Paktgeschenke = 30,
  Vampirismus = 31,
  Lykanthropie = 32,
  SkillStyles = 33,
  SkillExtended = 34,
  Magierkugel = 35,
  Hexenkessel = 36,
  Narrenkappe = 37,
  Schelmenspielzeug = 38,
  Alchimistenschale = 39,
  SexSchicksal = 40,
  Sex = 41,
  WaffenzauberAnimisten = 42,
  Sichelrituale = 43,
  Ringzauber = 44,
  Chronikzauber = 45,
}

export enum MagicalGroup {
  Spells = 1,
  Rituals = 2,
  Curses = 3,
  ElvenMagicalSongs = 4,
  MagicalMelodies = 5,
  MagicalDances = 6,
  DominationRituals = 7,
  RogueSpells = 8,
  AnimistForces = 9,
  GeodeRituals = 10,
  ZibiljaRituals = 11,
}

export enum MagicalTradition {
  General = 1,
  GuildMages = 2,
  Witches = 3,
  Elves = 4,
  Druids = 5,
  Scharlatane = 6,
  ArcaneBards = 7,
  ArcaneDancers = 8,
  IntuitiveZauberer = 9,
  Meistertalentierte = 10,
  Qabalyamagier = 11,
  Kristallomanten = 12,
  Geodes = 13,
  Alchimisten = 14,
  Rogues = 15,
  Animists = 16,
  Zibilija = 17,
  BrobimGeoden = 18,
}

export enum Property {
  AntiMagic = 1,
  Demonic = 2,
  Influence = 3,
  Elemental = 4,
  Healing = 5,
  Clairvoyance = 6,
  Illusion = 7,
  Spheres = 8,
  Objekt = 9,
  Telekinesis = 10,
  Transformation = 11,
  Temporal = 12,
}

export enum BlessedGroup {
  LiturgicalChants = 1,
  Ceremonies = 2,
}

export enum BlessedTradition {
  General = 1,
  ChurchOfPraios = 2,
  ChurchOfRondra = 3,
  ChurchOfBoron = 4,
  ChurchOfHesinde = 5,
  ChurchOfPhex = 6,
  ChurchOfPeraine = 7,
  ChurchOfEfferd = 8,
  ChurchOfTravia = 9,
  ChurchOfFirun = 10,
  ChurchOfTsa = 11,
  ChurchOfIngerimm = 12,
  ChurchOfRahja = 13,
  CultOfTheNamelessOne = 14,
  ChurchOfAves = 15,
  ChurchOfIfirn = 16,
  ChurchOfKor = 17,
  ChurchOfNandus = 18,
  ChurchOfSwafnir = 19,
  CultOfNuminoru = 20,
  Levthankult = 21,
}

export enum Aspect {
  General = 1,
  AntiMagic = 2,
  Order = 3,
  Shield = 4,
  Storm = 5,
  Death = 6,
  Dream = 7,
  Magic = 8,
  Knowledge = 9,
  Commerce = 10,
  Shadow = 11,
  Healing = 12,
  Agriculture = 13,
  Wind = 14,
  Wogen = 15,
  Freundschaft = 16,
  Heim = 17,
  Jagd = 18,
  Kaelte = 19,
  Freiheit = 20,
  Wandel = 21,
  Feuer = 22,
  Handwerk = 23,
  Ekstase = 24,
  Harmonie = 25,
  Reise = 26,
  Schicksal = 27,
  Hilfsbereitschaft = 28,
  Natur = 29,
  GuterKampf = 30,
  GutesGold = 31,
  Bildung = 32,
  Erkenntnis = 33,
  Kraft = 34,
  Tapferkeit = 35,
  ReissenderStrudel = 36,
  UnendlicheTiefe = 37,
  Begierde = 38,
  Rausch = 39,
}

export enum EquipmentGroup {
  MeleeWeapons = 1,
  RangedWeapons = 2,
  Ammunition = 3,
  Armor = 4,
  WeaponAccessories = 5,
  Clothes = 6,
  TravelGearAndTools = 7,
  Illumination = 8,
  BandagesAndRemedies = 9,
  Containers = 10,
  RopesAndChains = 11,
  ThievesTools = 12,
  ToolsOfTheTrade = 13,
  OrienteeringAids = 14,
  Jewelry = 15,
  GemsAndPreciousStones = 16,
  Stationary = 17,
  Books = 18,
  MagicalArtifacts = 19,
  Alchemicae = 20,
  Poisons = 21,
  HealingHerbs = 22,
  MusicalInstruments = 23,
  LuxuryGoods = 24,
  Animals = 25,
  AnimalCare = 26,
  Vehicles = 27,
  AusruestungDerGeweihtenschaft = 28,
  Zeremonialgegenstaende = 29,
  Liebesspielzeug = 30,
}

export enum ArmorType {
  NormalClothing = 1,
  HeavyClothing = 2,
  ClothArmor = 3,
  LeatherArmor = 4,
  WoodArmor = 5,
  ChainArmor = 6,
  ScaleArmor = 7,
  PlateArmor = 8,
  JoustingArmor = 9,
  Gestechrüstung = 10,
}
