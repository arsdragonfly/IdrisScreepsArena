import * as utils from '/game/utils';
import * as prototypes from '/game/prototypes';
import * as constants from '/game/constants';
import { } from '/arena';

Object.entries(utils).forEach(([name, exported]) => global[name] = exported);
Object.entries(prototypes).forEach(([name, exported]) => global[name] = exported);
Object.entries(constants).forEach(([name, exported]) => global[name] = exported);
