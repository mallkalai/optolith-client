import { last } from 'lodash';
import { AdventurePointsState, getLeft } from '../reducers/adventurePoints';
import { DependentInstancesState, get } from '../reducers/dependentInstances';
import { SpecialAbilityInstance } from '../types/data.d';

/**
 * Checks if there are enough AP available.
 * @param cost The AP value you want to check.
 * @param ap The current AP state.
 */
export function validate(cost: number, ap: AdventurePointsState): boolean {
	if (cost > 0) {
		return cost <= getLeft(ap);
	}
	return true;
}

export function checkDisAdvantages(state: DependentInstancesState, cost: number, index: number, target: number[], spent: number, total: number, add: boolean): [boolean, boolean, boolean] {
	const absCost = add ? cost : -cost;
	const smallMax = getAdvantagesDisadvantagesSubMax(state, index);
	const subValid = index > 0 ? target[index] + absCost <= smallMax : true;
	const mainValid = target[0] + absCost <= 80;
	const totalValid = spent + cost <= total;

	return [ totalValid, mainValid, subValid ];
}

/**
 * Returns the maximum AP value you can spend on magical/blessed advantages/disadvantages.
 * @param state The list of dependent instances.
 * @param index The index in the AP array. `0` equals General, `1` equals Magical and `2` equals Blessed.
 */
export function getAdvantagesDisadvantagesSubMax(state: DependentInstancesState, index: number): number {
	let max = 50;
	if (index === 1) {
		const traditionActive = last((get(state, 'SA_86') as SpecialAbilityInstance).active);
		if (traditionActive && typeof traditionActive.sid === 'number' && traditionActive.sid >= 6 && traditionActive.sid <= 9) {
			max = 25;
		}
	}
	return max;
}
