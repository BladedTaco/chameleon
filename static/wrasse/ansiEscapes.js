
import {clamp} from './util';

const ESC = '\u001B[';
const OSC = '\u001B]';
const BEL = '\u0007';
const SEP = ';';
const isTerminalApp = false;

const ansiEscapes = {};

ansiEscapes.cursorTo = (x, y) => {
	if (typeof x !== 'number') {
		throw new TypeError('The `x` argument is required');
	}

	if (typeof y !== 'number') {
		return ESC + (x + 1) + 'G';
	}

	return ESC + (y + 1) + ';' + (x + 1) + 'H';
};

ansiEscapes.cursorMove = (x, y) => {
	if (typeof x !== 'number') {
		throw new TypeError('The `x` argument is required');
	}

	let returnValue = '';

	if (x < 0) {
		returnValue += ESC + (-x) + 'D';
	} else if (x > 0) {
		returnValue += ESC + x + 'C';
	}

	if (y < 0) {
		returnValue += ESC + (-y) + 'A';
	} else if (y > 0) {
		returnValue += ESC + y + 'B';
	}

	return returnValue;
};

ansiEscapes.cursorUp = (count = 1) => ESC + count + 'A';
ansiEscapes.cursorDown = (count = 1) => ESC + count + 'B';
ansiEscapes.cursorForward = (count = 1) => ESC + count + 'C';
ansiEscapes.cursorBackward = (count = 1) => ESC + count + 'D';

ansiEscapes.cursorLeft = ESC + 'G';
ansiEscapes.cursorSavePosition = isTerminalApp ? '\u001B7' : ESC + 's';
ansiEscapes.cursorRestorePosition = isTerminalApp ? '\u001B8' : ESC + 'u';
ansiEscapes.cursorGetPosition = ESC + '6n';
ansiEscapes.cursorNextLine = ESC + 'E';
ansiEscapes.cursorPrevLine = ESC + 'F';
ansiEscapes.cursorHide = ESC + '?25l';
ansiEscapes.cursorShow = ESC + '?25h';

ansiEscapes.eraseLines = count => {
	let clear = '';

	for (let i = 0; i < count; i++) {
		clear += ansiEscapes.eraseLine + (i < count - 1 ? ansiEscapes.cursorUp() : '');
	}

	if (count) {
		clear += ansiEscapes.cursorLeft;
	}

	return clear;
};

ansiEscapes.eraseEndLine = ESC + 'K';
ansiEscapes.eraseStartLine = ESC + '1K';
ansiEscapes.eraseLine = ESC + '2K';
ansiEscapes.eraseDown = ESC + 'J';
ansiEscapes.eraseUp = ESC + '1J';
ansiEscapes.eraseScreen = ESC + '2J';
ansiEscapes.scrollUp = ESC + 'S';
ansiEscapes.scrollDown = ESC + 'T';

ansiEscapes.clearScreen = '\u001Bc';

ansiEscapes.beep = BEL;

ansiEscapes.link = (text, url) => {
	return [
		OSC,
		'8',
		SEP,
		SEP,
		url,
		BEL,
		text,
		OSC,
		'8',
		SEP,
		SEP,
		BEL
	].join('');
};

ansiEscapes.image = (buffer, options = {}) => {
	let returnValue = `${OSC}1337;File=inline=1`;

	if (options.width) {
		returnValue += `;width=${options.width}`;
	}

	if (options.height) {
		returnValue += `;height=${options.height}`;
	}

	if (options.preserveAspectRatio === false) {
		returnValue += ';preserveAspectRatio=0';
	}

	return returnValue + ':' + buffer.toString('base64') + BEL;
};

ansiEscapes.insertLine = (number) => {
	return ESC + number + "L";
}

ansiEscapes.deleteLine = (number) => {
	return ESC + number + "M";
}


ansiEscapes.cursorRow = (row) => {
	return ESC + row + 'd';
}
ansiEscapes.cursorPos = (row, col) => {
	return ESC + row + SEP + col + 'd';
}

ansiEscapes.colouredText = (fg_col, bg_col, text) => {
	// get colours with 0 in defaults
	let fg = {r:0, g:0, b:0, ...fg_col}
	let bg = {r:0, g:0, b:0, ...bg_col}
	// return text string
	return ESC + "38;2" + SEP + fg.r + SEP + fg.g + SEP + fg.b + "m"
		 + ESC + "48;2" + SEP + bg.r + SEP + bg.g + SEP + bg.b + "m"
		 + text 
		 + ESC + "39m" + ESC + "49m"
}

ansiEscapes.colourSeq = (col, isFG) => {
	// get colours with 0 in defaults
	isFG ??= false;
	let c = {r:0, g:0, b:0, ...col}
	// return text string
	return ESC + `${isFG ? '38' : '48'};2` + SEP + c.r + SEP + c.g + SEP + c.b + "m"
}


ansiEscapes.Colour = class Colour {
	static Red = new Colour({r: 255});
	static Blue = new Colour({b : 255});
	static Green = new Colour({g : 255});
	static Grey = new Colour({r:128, g:128, b:128});
	static LightGrey = new Colour({r:190, g:190, b:190});
	static White = new Colour({r:255, g:255, b:255});

	constructor({r, g, b}) {
		this.r = r ?? 0;
		this.g = g ?? 0;
		this.b = b ?? 0;
	}

	mul(multiplier) {
		return {
			r: Math.round(clamp(0, this.r * multiplier, 255)),
			g: Math.round(clamp(0, this.g * multiplier, 255)), 
			b: Math.round(clamp(0, this.b * multiplier, 255)) 
		}
	}
}


export default ansiEscapes;
