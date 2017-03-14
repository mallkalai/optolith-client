import * as ActionTypes from '../constants/ActionTypes';
import * as Categories from '../constants/Categories';
import AppDispatcher from '../dispatcher/AppDispatcher';
import * as ActivatableUtils from '../utils/ActivatableUtils';
import HistoryStore from './HistoryStore';
import { get, getAllByCategory } from './ListStore';
import RequirementsStore from './RequirementsStore';
import Store from './Store';

type Action = AddAttributePointAction | RemoveAttributePointAction | AddArcaneEnergyPointAction | AddKarmaPointAction | AddLifePointAction | ReceiveHeroDataAction | CreateHeroAction | UndoTriggerActions | RemovePermanentAEPointAction | RemovePermanentKPPointAction | RemoveRedeemedAEPointAction | RemoveRedeemedKPPointAction | RedeemAEPointAction | RedeemKPPointAction | SetSelectionsAction;
type ids = 'LP' | 'AE' | 'KP';

class AttributeStoreStatic extends Store {
	private readonly category: ATTRIBUTES = Categories.ATTRIBUTES;
	private addedLifePoints = 0;
	private addedArcaneEnergy = 0;
	private addedKarmaPoints = 0;
	private permanentArcaneEnergy = {
		lost: 0,
		redeemed: 0,
	};
	private permanentKarmaPoints = {
		lost: 0,
		redeemed: 0,
	};
	readonly dispatchToken: string;

	constructor() {
		super();
		this.dispatchToken = AppDispatcher.register((action: Action) => {
			AppDispatcher.waitFor([RequirementsStore.dispatchToken]);
			if (action.undo) {
				AppDispatcher.waitFor([HistoryStore.dispatchToken]);
				switch (action.type) {
					case ActionTypes.ADD_ATTRIBUTE_POINT:
					case ActionTypes.REMOVE_ATTRIBUTE_POINT:
					case ActionTypes.ACTIVATE_DISADV:
					case ActionTypes.DEACTIVATE_DISADV:
						break;

					case ActionTypes.ACTIVATE_SPECIALABILITY:
						this.changePermanentArcaneEnergyBySpecialAbility(action.payload.id, true);
						break;

					case ActionTypes.DEACTIVATE_SPECIALABILITY:
						this.changePermanentArcaneEnergyBySpecialAbility(action.payload.id);
						break;

					case ActionTypes.ADD_LIFE_POINT:
						this.removeLifePoint();
						break;

					case ActionTypes.ADD_ARCANE_ENERGY_POINT:
						this.removeArcaneEnergyPoint();
						break;

					case ActionTypes.ADD_KARMA_POINT:
						this.removeKarmaPoint();
						break;

					case ActionTypes.REDEEM_AE_POINT:
						this.removeRedeemedAEPoint();
						break;

					case ActionTypes.REDEEM_KP_POINT:
						this.removeRedeemedKPPoint();
						break;

					case ActionTypes.REMOVE_REDEEMED_AE_POINT:
						this.redeemAEPoint();
						break;

					case ActionTypes.REMOVE_REDEEMED_KP_POINT:
						this.redeemKPPoint();
						break;

					case ActionTypes.REMOVE_PERMANENT_AE_POINTS:
						this.addPermanentAEPoints(action.payload.value);
						break;

					case ActionTypes.REMOVE_PERMANENT_KP_POINTS:
						this.addPermanentKPPoints(action.payload.value);
						break;

					default:
						return true;
				}
			}
			else {
				switch (action.type) {
					case ActionTypes.CREATE_HERO:
						this.clear();
						break;

					case ActionTypes.RECEIVE_HERO_DATA:
						this.updateAll(action.payload.data.attr);
						break;

					case ActionTypes.ASSIGN_RCP_OPTIONS:
						this.assign();
						break;

					case ActionTypes.ADD_ATTRIBUTE_POINT:
					case ActionTypes.REMOVE_ATTRIBUTE_POINT:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						break;

					case ActionTypes.ADD_LIFE_POINT:
						if (RequirementsStore.isValid()) {
							this.addLifePoint();
						}
						break;

					case ActionTypes.ADD_ARCANE_ENERGY_POINT:
						if (RequirementsStore.isValid()) {
							this.addArcaneEnergyPoint();
						}
						break;

					case ActionTypes.ADD_KARMA_POINT:
						if (RequirementsStore.isValid()) {
							this.addKarmaPoint();
						}
						break;

					case ActionTypes.REDEEM_AE_POINT:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						if (RequirementsStore.isValid()) {
							this.redeemAEPoint();
						}
						break;

					case ActionTypes.REDEEM_KP_POINT:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						if (RequirementsStore.isValid()) {
							this.redeemKPPoint();
						}
						break;

					case ActionTypes.REMOVE_REDEEMED_AE_POINT:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						if (RequirementsStore.isValid()) {
							this.removeRedeemedAEPoint();
						}
						break;

					case ActionTypes.REMOVE_REDEEMED_KP_POINT:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						if (RequirementsStore.isValid()) {
							this.removeRedeemedKPPoint();
						}
						break;

					case ActionTypes.REMOVE_PERMANENT_AE_POINTS:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						this.removePermanentAEPoints(action.payload.value);
						break;

					case ActionTypes.REMOVE_PERMANENT_KP_POINTS:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						this.removePermanentKPPoints(action.payload.value);
						break;

					case ActionTypes.ACTIVATE_SPECIALABILITY:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						this.changePermanentArcaneEnergyBySpecialAbility(action.payload.id);
						break;

					case ActionTypes.DEACTIVATE_SPECIALABILITY:
						AppDispatcher.waitFor([HistoryStore.dispatchToken]);
						this.changePermanentArcaneEnergyBySpecialAbility(action.payload.id, true);
						break;

					default:
						return true;
				}
			}
			this.emitChange();
			return true;
		});
	}

	getAll() {
		return getAllByCategory(this.category) as AttributeInstance[];
	}

	getAdd(id: ids) {
		switch (id) {
			case 'LP':
				return this.addedLifePoints;
			case 'AE':
				return this.addedArcaneEnergy;
			case 'KP':
				return this.addedKarmaPoints;
		}
	}

	getAddEnergies() {
		return {
			ae: this.addedArcaneEnergy,
			kp: this.addedKarmaPoints,
			lp: this.addedLifePoints,
			permanentAE: this.permanentArcaneEnergy,
			permanentKP: this.permanentKarmaPoints,
		};
	}

	getSum() {
		return this.getAll().reduce((a, b) => a + b.value, 0);
	}

	getForSave() {
		return {
			values: this.getAll().map(e => [e.id, e.value, e.mod] as [string, number, number]),
			...this.getAddEnergies(),
		};
	}

	getPermanentRedeemedChangeAmount(id: string) {
		const value = this.getChangePermanentArcaneEnergyBySpecialAbilityAmount(id, true);
		const { redeemed, lost } = this.permanentArcaneEnergy;
		const balance = lost + value;
		return redeemed > balance ? redeemed - balance : 0;
	}

	private addLifePoint() {
		this.addedLifePoints++;
	}

	private addArcaneEnergyPoint() {
		this.addedArcaneEnergy++;
	}

	private addKarmaPoint() {
		this.addedKarmaPoints++;
	}

	private removeLifePoint() {
		this.addedLifePoints--;
	}

	private removeArcaneEnergyPoint() {
		this.addedArcaneEnergy--;
	}

	private removeKarmaPoint() {
		this.addedKarmaPoints--;
	}

	private redeemAEPoint() {
		this.permanentArcaneEnergy.redeemed++;
	}

	private redeemKPPoint() {
		this.permanentKarmaPoints.redeemed++;
	}

	private removeRedeemedAEPoint() {
		this.permanentArcaneEnergy.redeemed--;
	}

	private removeRedeemedKPPoint() {
		this.permanentKarmaPoints.redeemed--;
	}

	private removePermanentAEPoints(value: number) {
		this.permanentArcaneEnergy.lost += value;
	}

	private removePermanentKPPoints(value: number) {
		this.permanentKarmaPoints.lost += value;
	}

	private addPermanentAEPoints(value: number) {
		this.permanentArcaneEnergy.lost -= value;
	}

	private addPermanentKPPoints(value: number) {
		this.permanentKarmaPoints.lost -= value;
	}

	private clear() {
		this.addedLifePoints = 0;
		this.addedArcaneEnergy = 0;
		this.addedKarmaPoints = 0;
	}

	private updateAll(obj: { lp: number; ae: number; kp: number; permanentAE: { lost: number; redeemed: number; }; permanentKP: { lost: number; redeemed: number; }; }) {
		this.addedLifePoints = obj.lp;
		this.addedArcaneEnergy = obj.ae;
		this.addedKarmaPoints = obj.kp;
		this.permanentArcaneEnergy = obj.permanentAE;
		this.permanentKarmaPoints = obj.permanentKP;
	}

	private getChangePermanentArcaneEnergyBySpecialAbilityAmount = (id: string, negative: boolean = false) => {
		const modifier = negative ? -1 : 1;
		let value = 0;
		switch (id) {
			case 'SA_92':
				value = 2;
				break;
		}
		return value * modifier;
	}

	private changePermanentArcaneEnergyBySpecialAbility = (id: string, negative: boolean = false) => {
		const value = this.getChangePermanentArcaneEnergyBySpecialAbilityAmount(id, negative);
		this.permanentArcaneEnergy.lost += value;
		const { redeemed, lost } = this.permanentArcaneEnergy;
		if (redeemed > lost) {
			this.permanentArcaneEnergy.redeemed = lost;
		}
	}

	private assign() {
		if (ActivatableUtils.isActive(get('SA_92') as SpecialAbilityInstance)) {
			this.permanentArcaneEnergy.lost += 2;
		}
	}
}

const AttributeStore: AttributeStoreStatic = new AttributeStoreStatic();

export default AttributeStore;