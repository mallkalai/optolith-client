import { ident, thrush } from "../../Data/Function"
import { fmap, fmapF } from "../../Data/Functor"
import { cons, consF, elem, filter, fnull, List, map, maximum } from "../../Data/List"
import { fromJust, isJust, Just, liftM2, Maybe, maybe, Nothing, or } from "../../Data/Maybe"
import { add, divideBy, gt, multiply } from "../../Data/Num"
import { findWithDefault, foldrWithKey, lookup } from "../../Data/OrderedMap"
import { Record } from "../../Data/Record"
import { uncurryN, uncurryN5 } from "../../Data/Tuple/Curry"
import { IdPrefixes } from "../Constants/IdPrefixes"
import { AdvantageId, CombatTechniqueId, SpecialAbilityId } from "../Constants/Ids.gen"
import { ActivatableDependent } from "../Models/ActiveEntries/ActivatableDependent"
import { createSkillDependentWithValue6, SkillDependent } from "../Models/ActiveEntries/SkillDependent"
import { HeroModel, HeroModelRecord } from "../Models/Hero/HeroModel"
import { CombatTechniqueWithAttackParryBase, CombatTechniqueWithAttackParryBaseA_ } from "../Models/View/CombatTechniqueWithAttackParryBase"
import { CombatTechniqueWithRequirements } from "../Models/View/CombatTechniqueWithRequirements"
import { CombatTechnique } from "../Models/Wiki/CombatTechnique"
import { ExperienceLevel } from "../Models/Wiki/ExperienceLevel"
import { StaticData, StaticDataRecord } from "../Models/Wiki/WikiModel"
import { isMaybeActive } from "../Utilities/Activatable/isActive"
import { getActiveSelections } from "../Utilities/Activatable/selectionUtils"
import { createMaybeSelector } from "../Utilities/createMaybeSelector"
import { flattenDependencies } from "../Utilities/Dependencies/flattenDependencies"
import { filterAndSortRecordsBy } from "../Utilities/filterAndSortBy"
import { compareLocale } from "../Utilities/I18n"
import { prefixId } from "../Utilities/IDUtils"
import { pipe, pipe_ } from "../Utilities/pipe"
import { filterByAvailabilityAndPred, isEntryFromCoreBook } from "../Utilities/RulesUtils"
import { comparingR, sortByMulti } from "../Utilities/sortBy"
import { getMaxAttributeValueByID } from "./attributeSelectors"
import { getStartEl } from "./elSelectors"
import { getRuleBooksEnabled } from "./rulesSelectors"
import { getCombatTechniquesWithRequirementsSortOptions } from "./sortOptionsSelectors"
import { getAttributes, getCombatTechniques, getCombatTechniquesFilterText, getCurrentHeroPresent, getSpecialAbilities, getWiki, getWikiCombatTechniques } from "./stateSelectors"

const CTA = CombatTechnique.A
const SDA = SkillDependent.A
const CTWAPBA = CombatTechniqueWithAttackParryBase.A
const CTWRA = CombatTechniqueWithRequirements.A
const ADA = ActivatableDependent.A

const getMCBasis = //Melee Combat
(attributes: HeroModel["attributes"]) =>
  pipe_(
    List (prefixId (IdPrefixes.ATTRIBUTES) (6)),
    getMaxAttributeValueByID (attributes),
    add( 
      pipe_ (
        List (prefixId (IdPrefixes.ATTRIBUTES) (8)),
        getMaxAttributeValueByID (attributes)
      )
    )
  )

const getRCBasis = //Ranged Combat
(attributes: HeroModel["attributes"]) =>
  pipe_(
    List (prefixId (IdPrefixes.ATTRIBUTES) (5)),
    getMaxAttributeValueByID (attributes),
    multiply(2)
  )

const getAttackBase =
(attributes: HeroModel["attributes"]) => //attributes ist vom TYP OrdereMap, mit einem String als Key und den Attributsobjekten als Value, diese enthalten id, value etc.
(wiki_entry: Record<CombatTechnique>) => //wiki entry ist ein Record vom TYP CombatTechnique
(hero_entry: Record<SkillDependent>): number => //hero_entry ist ein Record vom Typ SkillDependent
  pipe_(
    CTA.gr (wiki_entry) === 2? 
    getRCBasis(attributes) : 
    getMCBasis(attributes),
    divideBy(3),
    Math.floor,
    add (SDA.value (hero_entry))
  )

const getParryBase =
  (attributes: HeroModel["attributes"]) =>
  (wiki_entry: Record<CombatTechnique>) =>
  (hero_entry: Record<SkillDependent>): Maybe<number> => {
    const curr_id = CTA.id (wiki_entry)
    const curr_gr = CTA.gr (wiki_entry)

    return curr_gr === 2
      || curr_id === prefixId (IdPrefixes.COMBAT_TECHNIQUES) (8)
        ? Nothing
        : pipe_(
          getMCBasis(attributes),
          divideBy(3),
          Math.floor,
          Just

        )
  }

export const getCombatTechniquesForView = createMaybeSelector (
  getWiki,
  getWikiCombatTechniques,
  getAttributes,
  getSpecialAbilities,
  getCombatTechniques,
  uncurryN5 (staticData =>
             wiki_combat_techniques =>
             attributes =>
             special_abilities =>
               fmap ((combatTechniques): List<Record<CombatTechniqueWithAttackParryBase>> =>
                 pipe_ (
                   wiki_combat_techniques,
                   foldrWithKey ((id: string) => (wiki_entry: Record<CombatTechnique>) => {
                                const hero_entry =
                                  findWithDefault (createSkillDependentWithValue6 (id))
                                                  (id)
                                                  (combatTechniques)

                                if (id === CombatTechniqueId.spittingFire
                                    && maybe (true)
                                             (pipe (ADA.active, fnull))
                                             (lookup (SpecialAbilityId.feuerschlucker)
                                                     (special_abilities))) {
                                  // If SF Feuerschlucker is not active, do not
                                  // show CT Spitting Fire
                                  return ident as
                                    ident<List<Record<CombatTechniqueWithAttackParryBase>>>
                                }

                                return consF (CombatTechniqueWithAttackParryBase ({
                                  at: getAttackBase (attributes) (wiki_entry) (hero_entry),
                                  pa: getParryBase (attributes) (wiki_entry) (hero_entry),
                                  stateEntry: hero_entry,
                                  wikiEntry: wiki_entry,
                                }))
                              })
                              (List.empty),
                   sortByMulti ([ comparingR (CombatTechniqueWithAttackParryBaseA_.name)
                                             (compareLocale (staticData)) ])
                 )))
)

export const getCombatTechniquesForSheet = createMaybeSelector (
  getWiki,
  getCombatTechniquesForView,
  uncurryN (staticData =>
             fmap (filter (x => SDA.value (CTWAPBA.stateEntry (x)) > 6
                                || isEntryFromCoreBook (CTA.src)
                                                       (StaticData.A.books (staticData))
                                                       (CTWAPBA.wikiEntry (x)))))
)

const getMaximum =
  (exceptionalCombatTechnique: Maybe<Record<ActivatableDependent>>) =>
  (startEl: Maybe<Record<ExperienceLevel>>) =>
  (attributes: HeroModel["attributes"]) =>
  (phase: number) =>
  (ct: Record<CombatTechniqueWithAttackParryBase>): number => {
    const curr_id = pipe_ (ct, CTWAPBA.wikiEntry, CTA.id)

    const isBonusValid = or (fmapF (exceptionalCombatTechnique)
                                   (pipe (getActiveSelections, elem<string | number> (curr_id))))

    const bonus = isBonusValid ? 1 : 0

    if (phase < 3 && isJust (startEl)) {
      return ExperienceLevel.A.maxCombatTechniqueRating (fromJust (startEl)) + bonus
    }

    const curr_primary = pipe_ (ct, CTWAPBA.wikiEntry, CTA.primary)

    return getMaxAttributeValueByID (attributes) (curr_primary) + 2 + bonus
  }

const getMinimum =
  (hunterRequiresMinimum: boolean) =>
  (wiki: StaticDataRecord) =>
  (hero: HeroModelRecord) =>
  (ct: Record<CombatTechniqueWithAttackParryBase>): number => {
    const curr_dependencies = pipe_ (ct, CTWAPBA.stateEntry, SDA.dependencies)
    const curr_gr = pipe_ (ct, CTWAPBA.wikiEntry, CTA.gr)

    const maxList = cons (flattenDependencies (wiki) (hero) (curr_dependencies))
                         (6)

    if (hunterRequiresMinimum && curr_gr === 2) {
      return maximum (cons (maxList) (10))
    }

    return maximum (maxList)
  }

const getGr = pipe (CTWAPBA.wikiEntry, CTA.gr)
const getValue = pipe (CTWAPBA.stateEntry, SDA.value)
type CTWAPB = CombatTechniqueWithAttackParryBase

export const getAllCombatTechniques = createMaybeSelector (
  getCombatTechniquesForView,
  getCurrentHeroPresent,
  getStartEl,
  getWiki,
  (mcombat_techniques, mhero, mstartEl, wiki) =>
    liftM2 ((combatTechniques: List<Record<CTWAPB>>) => (hero: HeroModelRecord) => {
             const exceptionalCombatTechnique =
              lookup<string> (AdvantageId.exceptionalCombatTechnique)
                             (HeroModel.A.advantages (hero))

             const hunter = lookup<string> (SpecialAbilityId.hunter)
                                           (HeroModel.A.specialAbilities (hero))

             const hunterRequiresMinimum =
               isMaybeActive (hunter)
               && thrush (combatTechniques) (List.any (x => getGr (x) === 2 && getValue (x) >= 10))

             return thrush (combatTechniques)
                           (map (x =>
                             CombatTechniqueWithRequirements ({
                               at: CTWAPBA.at (x),
                               pa: CTWAPBA.pa (x),
                               min: getMinimum (hunterRequiresMinimum)
                                               (wiki)
                                               (hero)
                                               (x),
                               max: getMaximum (exceptionalCombatTechnique)
                                               (mstartEl)
                                               (HeroModel.A.attributes (hero))
                                               (HeroModel.A.phase (hero))
                                               (x),
                               stateEntry: CTWAPBA.stateEntry (x),
                               wikiEntry: CTWAPBA.wikiEntry (x),
                             })))
           })
           (mcombat_techniques)
           (mhero)
)

export const getAvailableCombatTechniques = createMaybeSelector (
  getRuleBooksEnabled,
  getAllCombatTechniques,
  uncurryN (av => fmap (filterByAvailabilityAndPred (pipe (CTWRA.wikiEntry, CTA.src))
                                                    (pipe (CTWRA.stateEntry, SDA.value, gt (6)))
                                                    (av)))
)

export const getFilteredCombatTechniques = createMaybeSelector (
  getAvailableCombatTechniques,
  getCombatTechniquesWithRequirementsSortOptions,
  getCombatTechniquesFilterText,
  (mcombat_techniques, sortOptions, filterText) =>
    fmapF (mcombat_techniques)
          (filterAndSortRecordsBy (0)
                                  ([ pipe (CTWRA.wikiEntry, CTA.name) ])
                                  (sortOptions)
                                  (filterText))
)
