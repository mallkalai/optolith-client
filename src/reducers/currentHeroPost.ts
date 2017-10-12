import { flatten, isEqual } from 'lodash';
import { CreateHeroAction } from '../actions/HerolistActions';
import { SetSelectionsAction } from '../actions/ProfessionActions';
import * as ActionTypes from '../constants/ActionTypes';
import * as Categories from '../constants/Categories';
import { DisAdvAdventurePoints } from '../reducers/adventurePoints';
import { get, getLatest } from '../selectors/dependentInstancesSelectors';
import { getStart } from '../selectors/elSelectors';
import * as Data from '../types/data.d';
import * as Reusable from '../types/reusable.d';
import * as ActivatableUtils from '../utils/ActivatableUtils';
import * as DependentUtils from '../utils/DependentUtils';
import { getDecreaseRangeAP, getIncreaseAP, getIncreaseRangeAP } from '../utils/ICUtils';
import { mergeIntoState, setNewStateItem, setStateItem } from '../utils/ListUtils';
import * as RCPUtils from '../utils/RCPUtils';
import * as RequirementUtils from '../utils/RequirementUtils';
import { addStyleExtendedSpecialAbilityDependencies } from './activatable';
import { CurrentHeroInstanceState } from './currentHero';
import { DependentInstancesState } from './dependentInstances';

type Action = CreateHeroAction | SetSelectionsAction;

export function currentHeroPost(state: CurrentHeroInstanceState, action: Action): CurrentHeroInstanceState {
  switch (action.type) {
    case ActionTypes.CREATE_HERO:
      return {
        ...state,
        ap: {
          total: getStart(state.el).ap,
          spent: 0,
          adv: [0, 0, 0],
          disadv: [0, 0, 0]
        }
      };

    case ActionTypes.ASSIGN_RCP_OPTIONS: {
      const { dependent, el, rcp } = state;

      const race = rcp.race && get(dependent, rcp.race) as Data.RaceInstance;
      const culture = rcp.culture && get(dependent, rcp.culture) as Data.CultureInstance;
      const profession = rcp.profession && get(dependent, rcp.profession) as Data.ProfessionInstance;
      const professionVariant = rcp.professionVariant && get(dependent, rcp.professionVariant) as Data.ProfessionVariantInstance;

      const skillRatingList = new Map<string, number>();
      const addToSkillRatingList = (id: string, value: number) => {
        const currentValue = skillRatingList.get(id);
        if (currentValue) {
          skillRatingList.set(id, currentValue + value);
          if (skillRatingList.get(id) === 0) {
            skillRatingList.delete(id);
          }
        }
        else {
          skillRatingList.set(id, value);
        }
      };
      const skillActivateList = new Set<string>();
      const activatable = new Set<Reusable.ProfessionRequiresActivatableObject>();
      const languages = new Map<number, number>();
      const scripts = new Set<number>();

      // Race selections:

      if (typeof race === 'object') {
        race.attributeAdjustments.forEach(e => {
          const [ mod, id ] = e;
          (get(dependent, id) as Data.AttributeInstance).mod += mod;
        });
        race.automaticAdvantages.forEach(e => activatable.add({ id: e, active: true }));
        (get(dependent, action.payload.attrSel) as Data.AttributeInstance).mod = race.attributeAdjustmentsSelection[0];
      }

      // Culture selections:

      if (typeof culture === 'object') {
        if (action.payload.useCulturePackage) {
          culture.talents.forEach(([ key, value ]) => {
            skillRatingList.set(key, value);
          });
        }

        const motherTongueId = culture.languages.length > 1 ? action.payload.lang : culture.languages[0];
        languages.set(motherTongueId, 4);

        if (action.payload.buyLiteracy) {
          const motherTongueScriptId = culture.scripts.length > 1 ? action.payload.litc : culture.scripts[0];
          scripts.add(motherTongueScriptId);
        }
      }

      // Profession selections:

      if (typeof profession === 'object') {
        [ ...profession.talents, ...profession.combatTechniques ].forEach(([ key, value ]) => {
          addToSkillRatingList(key, value);
        });
        [ ...profession.spells, ...profession.liturgies ].forEach(([ key, value ]) => {
          skillActivateList.add(key);
          addToSkillRatingList(key, value);
        });
        profession.blessings.forEach(e => skillActivateList.add(e));
        profession.specialAbilities.forEach(e => activatable.add(e));
      }

      if (typeof professionVariant === 'object') {
        [ ...professionVariant.talents, ...professionVariant.combatTechniques ].forEach(([ key, value ]) => {
          addToSkillRatingList(key, value);
        });
        [ ...professionVariant.spells, ...professionVariant.liturgies ].forEach(([ key, value ]) => {
          skillActivateList.add(key);
          addToSkillRatingList(key, value);
        });
        professionVariant.specialAbilities.forEach(e => {
          if (e.active === false) {
            activatable.forEach(i => {
              if (i.id === e.id) {
                activatable.delete(i);
              }
            });
          }
          else {
            activatable.add(e);
          }
        });
      }

      if (action.payload.map.has('SPECIALISATION')) {
        const { map, spec, specTalentId } = action.payload;
        const talentId = (map.get('SPECIALISATION') as Data.SpecialisationSelection).sid;
        if (Array.isArray(talentId) && specTalentId) {
          activatable.add({
            id: 'SA_9',
            active: true,
            sid: specTalentId,
            sid2: spec,
          });
        }
        else if (typeof talentId === 'string') {
          activatable.add({
            id: 'SA_9',
            active: true,
            sid: talentId,
            sid2: spec,
          });
        }
      }

      action.payload.langLitc.forEach((value, key) => {
        const [ category, id ] = key.split('_');
        if (category === 'LANG') {
          languages.set(Number.parseInt(id), value / 2);
        }
        else {
          scripts.add(Number.parseInt(id));
        }
      });

      action.payload.combattech.forEach(e => {
        addToSkillRatingList(e, (action.payload.map.get('COMBAT_TECHNIQUES') as Data.CombatTechniquesSelection).value);
      });

      action.payload.combatTechniquesSecond.forEach(e => {
        addToSkillRatingList(e, (action.payload.map.get('COMBAT_TECHNIQUES_SECOND') as Data.CombatTechniquesSecondSelection).value);
      });

      action.payload.cantrips.forEach(e => {
        skillActivateList.add(e);
      });

      action.payload.curses.forEach((value, key) => {
        addToSkillRatingList(key, value);
        skillActivateList.add(key);
      });

      action.payload.skills.forEach((value, key) => {
        const skill = state.dependent.talents.get(key);
        if (skill !== undefined) {
          addToSkillRatingList(key, value / skill.ic);
        }
      });

      // Apply:

      let newlist: Data.ToOptionalKeys<DependentInstancesState> = {};
      let calculatedIncreasableCost = 0;
      let calculatedActivatableCost = 0;

      function addValue(instance: Data.SkillishInstance, value: number): Data.SkillishInstance {
        calculatedIncreasableCost += getIncreaseRangeAP(instance.ic, instance.value, instance.value + value);
        return {
          ...instance,
          value: instance.value + value
        };
      }

      for (const [id, value] of skillRatingList) {
        newlist = setNewStateItem(newlist, id, addValue(getLatest(dependent, newlist, id) as Data.SkillishInstance, value));
      }

      function activate(instance: Data.ActivatableSkillishInstance): Data.ActivatableSkillishInstance {
        if (instance.category === Categories.BLESSINGS || instance.category === Categories.CANTRIPS) {
          calculatedIncreasableCost += 1;
        }
        else {
          calculatedIncreasableCost += getIncreaseAP(instance.ic);
        }
        return {
          ...instance,
          active: true
        };
      }

      for (const id of skillActivateList) {
        newlist = setNewStateItem(newlist, id, activate(getLatest(dependent, newlist, id) as Data.ActivatableSkillishInstance));
      }

      let fulllist = mergeIntoState(dependent, newlist);

      for (const req of activatable) {
        const { id, sid, sid2, tier } = req;
        const entry = get(fulllist, id as string) as Data.ActivatableInstance;
        const { cost } = ActivatableUtils.convertPerTierCostToFinalCost(ActivatableUtils.getNameCost({ id, sid, sid2, tier, index: 0 }, dependent, true));
        calculatedActivatableCost += cost;
        const adds = ActivatableUtils.getGeneratedPrerequisites(entry, { sid, sid2, tier }, true);
        const obj: Data.ActivatableInstance = {...entry, active: [...entry.active, { sid, sid2, tier }]};
        if (obj.category === Categories.SPECIAL_ABILITIES) {
          fulllist = addStyleExtendedSpecialAbilityDependencies(fulllist, obj);
        }
        const firstState = setStateItem(fulllist, obj.id, obj);
        const prerequisites = Array.isArray(obj.reqs) ? obj.reqs : flatten(tier && [...obj.reqs].filter(e => e[0] <= tier).map(e => e[1]) || []);
        fulllist = mergeIntoState(firstState, DependentUtils.addDependencies(firstState, [...prerequisites, ...adds], obj.id));
      }

      const SA_27 = get(fulllist, 'SA_27') as Data.SpecialAbilityInstance;
      const SA_29 = get(fulllist, 'SA_29') as Data.SpecialAbilityInstance;

      fulllist = setStateItem(fulllist, 'SA_27', {
        ...SA_27,
        active: [ ...SA_27.active, ...Array.from(scripts.values(), sid => ({ sid }))]
      });
      fulllist = setStateItem(fulllist, 'SA_29', {
        ...SA_29,
        active: [ ...SA_29.active, ...Array.from(languages.entries(), ([sid, tier]) => ({ sid, tier }))]
      });

      // AP

      let ap;
      let professionName;
      let permanentArcaneEnergyLoss = 0;

      if (race && culture && profession) {
        ap = {
          spent: state.ap.spent + calculatedIncreasableCost - profession.ap,
          adv: race.automaticAdvantagesCost,
          disadv: [0, 0, 0] as [number, number, number]
        };

        if (action.payload.buyLiteracy) {
          const id = culture.scripts.length > 1 ? action.payload.litc : culture.scripts[0];
          const selectionItem = ActivatableUtils.getSelectionItem(get(fulllist, 'SA_27') as Data.SpecialAbilityInstance, id);
          ap.spent += selectionItem && selectionItem.cost || 0;
        }

        if (profession && profession.id !== 'P_0') {
          const requires = [ ...profession.requires ];

          for (const [key, options] of action.payload.map) {
            if (RCPUtils.isLanguagesScriptsSelection(key, options)) {
              ap.spent += options.value;
            }
          }

          if (professionVariant) {
            ap.spent -= professionVariant.ap;
            requires.push(...professionVariant.requires);
          }

          // Test case
          if (profession.apOfActivatables + (professionVariant ? professionVariant.apOfActivatables : 0) !== calculatedActivatableCost) {
            alert(`Calculated different AP value. Do not continue character creation with this profession! ${profession && profession.id} ${professionVariant && professionVariant.id} ${profession.apOfActivatables + (professionVariant ? professionVariant.apOfActivatables : 0)} ${calculatedActivatableCost}`);
          }

          ap.spent += calculatedActivatableCost;

          // Assign profession requirements

          ap = requires.reduce((final, req) => {
            if (RequirementUtils.isRequiringIncreasable(req)) {
              const { id, value } = req;
              if (typeof id === 'string') {
                const obj = get(fulllist, id) as Data.AttributeInstance | Data.TalentInstance;
                switch (obj.category) {
                  case Categories.ATTRIBUTES: {
                    if (typeof value === 'number') {
                      fulllist = setStateItem(fulllist, id, { ...obj, value });
                      return { ...final, spent: final.spent + getIncreaseRangeAP(5, 8, value)};
                    }
                    return final;
                  }
                  case Categories.TALENTS: {
                    if (typeof value === 'number') {
                      fulllist = setStateItem(fulllist, id, { ...obj, value });
                      return { ...final, spent: final.spent + getIncreaseRangeAP(obj.ic, obj.value, value)};
                    }
                    return final;
                  }
                }
              }
            }
            else {
              const { id, sid, sid2, tier } = req;
              if (typeof id === 'string') {
                const obj = get(fulllist, id) as Data.ActivatableInstance & { tiers?: number };
                const activeObject = { sid: sid as string | number | undefined, sid2, tier };
                let costObj: {
                  spent: number;
                  adv: DisAdvAdventurePoints;
                  disadv: DisAdvAdventurePoints;
                } | number | undefined;

                const checkIfActive = (e: Data.ActiveObject) => isEqual(activeObject, e);

                if (!obj.active.find(checkIfActive)) {
                  fulllist = setStateItem(fulllist, id, { ...obj, active: [...obj.active, activeObject]});
                  const adds = ActivatableUtils.getGeneratedPrerequisites(obj, activeObject, true);
                  const prerequisites = Array.isArray(obj.reqs) ? obj.reqs : flatten(tier && [...obj.reqs].filter(e => e[0] <= tier).map(e => e[1]) || []);
                  if (obj.category === Categories.SPECIAL_ABILITIES) {
                    fulllist = addStyleExtendedSpecialAbilityDependencies(fulllist, obj);
                  }
                  fulllist = mergeIntoState(fulllist, DependentUtils.addDependencies(fulllist, [...prerequisites, ...adds], obj.id));
                  const { cost } = ActivatableUtils.convertPerTierCostToFinalCost(ActivatableUtils.getNameCost({ id, sid, sid2, tier, index: 0 }, dependent, true));
                  if (cost && (obj.category === Categories.ADVANTAGES || obj.category === Categories.DISADVANTAGES)) {
                    const isKar = RequirementUtils.getFlatFirstTierPrerequisites(obj.reqs).some(e => e !== 'RCP' && e.id === 'ADV_12' && RequirementUtils.isRequiringActivatable(e) && e.active);
                    const isMag = RequirementUtils.getFlatFirstTierPrerequisites(obj.reqs).some(e => e !== 'RCP' && e.id === 'ADV_50' && RequirementUtils.isRequiringActivatable(e) && e.active);
                    const index = isKar ? 2 : isMag ? 1 : 0;

                    costObj = {
                      adv: [0, 0, 0],
                      disadv: [0, 0, 0],
                      spent: cost,
                    };

                    if (obj.category === Categories.ADVANTAGES) {
                      costObj.adv[0] = costObj.spent;
                      if (index > 0) {
                        costObj.adv[index] = costObj.spent;
                      }
                    }
                    else {
                      costObj.disadv[0] = -costObj.spent;
                      if (index > 0) {
                        costObj.disadv[index] = -costObj.spent;
                      }
                    }
                  }
                  else {
                    costObj = cost;
                  }
                  if (typeof costObj === 'object') {
                    return {
                      adv: costObj.adv.map((e, i) => e + final.adv[i]) as [number, number, number],
                      disadv: costObj.disadv.map((e, i) => e + final.disadv[i]) as [number, number, number],
                      spent: final.spent + costObj.spent
                    };
                  }
                  else if (typeof costObj === 'number') {
                    return { ...final, spent: final.spent + cost};
                  }
                }
                return final;
              }
            }
            return final;
          }, ap);

          // Lower Combat Techniques with a too high CTR

          const maxCombatTechniqueRating = getStart(el).maxCombatTechniqueRating;
          const valueTooHigh = [...fulllist.combatTechniques.values()].filter(e => e.value > maxCombatTechniqueRating);

          ap.spent += valueTooHigh.reduce<number>((ap, instance) => {
            return ap + getDecreaseRangeAP(instance.ic, instance.value, maxCombatTechniqueRating);
          }, 0);

          for (const combatTechnique of valueTooHigh) {
            fulllist = setStateItem(fulllist, combatTechnique.id, { ...combatTechnique, value: maxCombatTechniqueRating });
          }

          if (rcp.profession === 'P_0') {
            professionName = 'Eigene Profession';
          }

          if (ActivatableUtils.isActive(get(fulllist, 'SA_76') as Data.SpecialAbilityInstance)) {
            permanentArcaneEnergyLoss += 2;
          }
        }
      }

      return {
        ...state,
        dependent: fulllist,
        ap: {
          ...state.ap,
          ...ap
        },
        energies: {
          ...state.energies,
          permanentArcaneEnergy: {
            ...state.energies.permanentArcaneEnergy,
            lost: state.energies.permanentArcaneEnergy.lost + permanentArcaneEnergyLoss
          }
        },
        profile: {
          ...state.profile,
          professionName
        }
      };
    }

    default:
      return state;
  }
}
