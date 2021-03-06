/* eslint "@typescript-eslint/type-annotation-spacing": [2, { "before": true, "after": true }] */
import { second } from "../../../../Data/Either"
import { fromMap } from "../../../../Data/OrderedMap"
import { Record } from "../../../../Data/Record"
import { NumIdName } from "../../../Models/NumIdName"
import { pipe } from "../../pipe"
import { map } from "../Array"
import { toMapIntegrity } from "../EntityIntegrity"
import { ReachL10n } from "../Schema/Reaches/Reaches.l10n"
import { YamlFileConverter } from "../ToRecordsByFile"


const toReach : (x : ReachL10n) => [number, Record<NumIdName>]
              = x => [ x.id, NumIdName (x) ]


export const toReaches : YamlFileConverter<number, Record<NumIdName>>
                       = pipe (
                           yaml_mp => yaml_mp.ReachesL10n,
                           map (toReach),
                           toMapIntegrity,
                           second (fromMap)
                         )
