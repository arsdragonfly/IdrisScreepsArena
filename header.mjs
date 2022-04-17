import * as utils from '/game/utils';
import { } from '/game/prototypes';
import { } from '/game/constants';
import { } from '/arena';

Object.entries(utils).forEach(([name, exported]) => global[name] = exported);
