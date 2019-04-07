import { List } from "../../../Data/List";
import { Maybe, Nothing } from "../../../Data/Maybe";
import { fromDefault, makeLenses, Record } from "../../../Data/Record";
import { Categories } from "../../Constants/Categories";
import { ProfessionRequireActivatable } from "./prerequisites/ActivatableRequirement";
import { ProfessionSelections } from "./professionSelections/ProfessionAdjustmentSelections";
import { IncreaseSkill } from "./sub/IncreaseSkill";
import { NameBySex } from "./sub/NameBySex";
import { SourceLink } from "./sub/SourceLink";
import { EntryWithCategory, ProfessionDependency, ProfessionPrerequisite } from "./wikiTypeHelpers";

export interface Profession {
  id: string
  name: string | Record<NameBySex>
  subname: Maybe<string | Record<NameBySex>>
  ap: number
  dependencies: List<ProfessionDependency>
  prerequisites: List<ProfessionPrerequisite>
  prerequisitesStart: Maybe<string>
  prerequisitesEnd: Maybe<string>
  selections: Record<ProfessionSelections>
  specialAbilities: List<Record<ProfessionRequireActivatable>>
  combatTechniques: List<Record<IncreaseSkill>>
  skills: List<Record<IncreaseSkill>>
  spells: List<Record<IncreaseSkill>>
  liturgicalChants: List<Record<IncreaseSkill>>
  blessings: List<string>
  suggestedAdvantages: List<string>
  suggestedAdvantagesText: Maybe<string>
  suggestedDisadvantages: List<string>
  suggestedDisadvantagesText: Maybe<string>
  unsuitableAdvantages: List<string>
  unsuitableAdvantagesText: Maybe<string>
  unsuitableDisadvantages: List<string>
  unsuitableDisadvantagesText: Maybe<string>
  isVariantRequired: boolean
  variants: List<string>
  category: Categories
  gr: number
  /**
   * Divides the groups into smaller subgroups, e.g. "Mage", "Blessed One of the
   * Twelve Gods" or "Fighter".
   */
  subgr: number
  src: List<Record<SourceLink>>
}

export const Profession =
  fromDefault<Profession> ({
    id: "",
    name: "",
    subname: Nothing,
    ap: 0,
    dependencies: List.empty,
    prerequisites: List.empty,
    prerequisitesStart: Nothing,
    prerequisitesEnd: Nothing,
    selections: ProfessionSelections.default,
    specialAbilities: List.empty,
    combatTechniques: List.empty,
    skills: List.empty,
    spells: List.empty,
    liturgicalChants: List.empty,
    blessings: List.empty,
    suggestedAdvantages: List.empty,
    suggestedAdvantagesText: Nothing,
    suggestedDisadvantages: List.empty,
    suggestedDisadvantagesText: Nothing,
    unsuitableAdvantages: List.empty,
    unsuitableAdvantagesText: Nothing,
    unsuitableDisadvantages: List.empty,
    unsuitableDisadvantagesText: Nothing,
    isVariantRequired: false,
    variants: List.empty,
    category: Categories.PROFESSIONS,
    gr: 0,
    subgr: 0,
    src: List.empty,
  })

export const ProfessionL = makeLenses (Profession)

export const isProfession =
  (r: EntryWithCategory) => Profession.AL.category (r) === Categories.PROFESSIONS
