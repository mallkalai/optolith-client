import { equals } from "../../Data/Eq"
import { ident } from "../../Data/Function"
import { over, set } from "../../Data/Lens"
import { bind, fromJust, isJust, Just, maybe } from "../../Data/Maybe"
import { adjust, any, deleteLookupWithKey, insert, lookup, sdelete } from "../../Data/OrderedMap"
import { Record } from "../../Data/Record"
import { fst, snd } from "../../Data/Tuple"
import * as HerolistActions from "../Actions/HerolistActions"
import * as IOActions from "../Actions/IOActions"
import * as ActionTypes from "../Constants/ActionTypes"
import { getInitialHeroObject, HeroModel, HeroModelL } from "../Models/Hero/HeroModel"
import { HeroesState, HeroesStateL } from "../Models/HeroesState"
import { composeL } from "../Utilities/compose"
import { pipe } from "../Utilities/pipe"
import { reduceReducersC } from "../Utilities/reduceReducers"
import { UndoState } from "../Utilities/undo"
import { heroReducer, toHeroWithHistory } from "./heroReducer"

type Action = IOActions.ReceiveInitialDataAction
            | IOActions.ReceiveImportedHeroAction
            | HerolistActions.CreateHeroAction
            | HerolistActions.LoadHeroAction
            | HerolistActions.SaveHeroAction
            | HerolistActions.DeleteHeroAction
            | HerolistActions.DuplicateHeroAction
            | HerolistActions.UpdateDateModifiedAction

export const precedingHerolistReducer =
  (action: Action): ident<Record<HeroesState>> => {
    switch (action.type) {
      case ActionTypes.CREATE_HERO: {
        const {
          l10n,
          el,
          enableAllRuleBooks,
          enabledRuleBooks,
          id,
          name,
          sex,
          totalAp,
        } = action.payload

        const hero = getInitialHeroObject (l10n)
                                          (id)
                                          (name)
                                          (sex)
                                          (el)
                                          (totalAp)
                                          (enableAllRuleBooks)
                                          (enabledRuleBooks)

        return pipe (
          set (HeroesStateL.currentId) (Just (id)),
          over (HeroesStateL.heroes)
               (insert (id)
                       (toHeroWithHistory (hero)))
        )
      }

      case ActionTypes.LOAD_HERO:
        return set (HeroesStateL.currentId) (Just (action.payload.id))

      case ActionTypes.DELETE_HERO: {
        const { id } = action.payload

        return state => {
          const heroes = HeroesState.AL.heroes (state)
          const delAndRemaining = deleteLookupWithKey (id) (heroes)

          const playerIdFromHero = pipe (heroReducer.A.present, HeroModel.AL.player)

          const playerId = bind (fst (delAndRemaining)) (playerIdFromHero)

          const hasUserMultipleHeroes = any (pipe (playerIdFromHero, equals (playerId)))
                                            (snd (delAndRemaining))


          if (isJust (playerId) && !hasUserMultipleHeroes) {
            return pipe (
                          over (HeroesStateL.users)
                               (sdelete (fromJust (playerId))),
                          set (HeroesStateL.heroes)
                              (snd (delAndRemaining))
                        )
                        (state)
          }

          return set (HeroesStateL.heroes)
                     (snd (delAndRemaining))
                     (state)
        }
      }

      case ActionTypes.DUPLICATE_HERO: {
        const { id, newId } = action.payload

        return state =>
          maybe
            (state)
            ((hero: Record<UndoState<Record<HeroModel>>>) =>
              over (HeroesStateL.heroes)
                   (insert (newId)
                           (toHeroWithHistory (pipe (
                                                      set (HeroModelL.id) (newId),
                                                      over (HeroModelL.name)
                                                           (name => `${name} (2)`)
                                                    )
                                                    (heroReducer.A.present (hero)))))
                   (state))
            (lookup (id) (HeroesState.AL.heroes (state)))
      }

      case ActionTypes.UPDATE_DATE_MODIFIED: {
        const { id, dateModified } = action.payload

        return over (HeroesStateL.heroes)
                    (adjust (set (composeL (heroReducer.L.present, HeroModelL.dateModified))
                                 (dateModified))
                            (id))
      }

      default:
        return ident
    }
  }

const prepareHeroReducer =
  (action: Action) =>
  (state: Record<HeroesState>): Record<HeroesState> =>
    maybe (state)
          ((current_id: string) => over (HeroesStateL.heroes)
                                        (adjust (heroReducer (action))
                                                (current_id))
                                        (state))
          (HeroesState.AL.currentId (state))

export const herolistReducer = reduceReducersC (
  precedingHerolistReducer,
  prepareHeroReducer
)
