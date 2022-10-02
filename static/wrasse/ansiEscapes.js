// trimmed version of the ansiescapes npm package with minor edits.

import {clamp} from './util';

const ansiEscapes = {};

ansiEscapes.LIGHT_MODE = true;

ansiEscapes.FGCol = {r:255,g:255,b:255}
ansiEscapes.BGCol = {r:0,g:0,b:0}

if (ansiEscapes.LIGHT_MODE) {
	[ansiEscapes.FGCol, ansiEscapes.BGCol] = [ansiEscapes.BGCol, ansiEscapes.FGCol]
}


// string constants for escape sequences
const ESC = '\u001B[';
const OSC = '\u001B]';
const BEL = '\u0007';
const SEP = ';';
const isTerminalApp = false;


// moves the cursor to the specified position in the terminal, top left is (1,1)
ansiEscapes.cursorTo = (x, y) => {
	if (typeof x !== 'number') {
		throw new TypeError('The `x` argument is required');
	}

	if (typeof y !== 'number') {
		return ESC + (x + 1) + 'G';
	}

	return ESC + (y + 1) + ';' + (x + 1) + 'H';
};

// moves the cursor relative to its current position
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

// set of functions to move the cursor in a single direction
ansiEscapes.cursorUp = (count = 1) => ESC + count + 'A';
ansiEscapes.cursorDown = (count = 1) => ESC + count + 'B';
ansiEscapes.cursorForward = (count = 1) => ESC + count + 'C';
ansiEscapes.cursorBackward = (count = 1) => ESC + count + 'D';

// constants for cursor manipulation
ansiEscapes.cursorLeft = ESC + 'G';
ansiEscapes.cursorSavePosition = isTerminalApp ? '\u001B7' : ESC + 's';
ansiEscapes.cursorRestorePosition = isTerminalApp ? '\u001B8' : ESC + 'u';
ansiEscapes.cursorGetPosition = ESC + '6n';
ansiEscapes.cursorNextLine = ESC + 'E';
ansiEscapes.cursorPrevLine = ESC + 'F';
ansiEscapes.cursorHide = ESC + '?25l';
ansiEscapes.cursorShow = ESC + '?25h';

// erases the given number of lines from the terminal
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

// constants for various erasure sequences
ansiEscapes.eraseEndLine = ESC + 'K';
ansiEscapes.eraseStartLine = ESC + '1K';
ansiEscapes.eraseLine = ESC + '2K';
ansiEscapes.eraseDown = ESC + 'J';
ansiEscapes.eraseUp = ESC + '1J';
ansiEscapes.eraseScreen = ESC + '2J';
// scrolling sequences
ansiEscapes.scrollUp = ESC + 'S';
ansiEscapes.scrollDown = ESC + 'T';

ansiEscapes.clearScreen = '\u001Bc';

// produces terminal beep
ansiEscapes.beep = BEL;

// creates a hypertext link
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

// embeds an image
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

// inserts n new lines at current cursor position
ansiEscapes.insertLine = (number) => {
	return ESC + number + "L";
}

// deletes n lines from the current cursor position
ansiEscapes.deleteLine = (number) => {
	return ESC + number + "M";
}

// sets the position of the cursor 0 indexed.
ansiEscapes.cursorRow = (row) => {
	return ESC + row + 'd';
}
ansiEscapes.cursorPos = (row, col) => {
	return ESC + row + SEP + col + 'd';
}

// colourizes the text with the specified foreground and background colour
ansiEscapes.colouredText = (fg_col, bg_col, text) => {
	// get colours with 0 in defaults
	let fg = {...ansiEscapes.FGCol, ...fg_col}
	let bg = {...ansiEscapes.BGCol, ...bg_col}
	// return text string
	return ESC + "38;2" + SEP + fg.r + SEP + fg.g + SEP + fg.b + "m"
		 + ESC + "48;2" + SEP + bg.r + SEP + bg.g + SEP + bg.b + "m"
		 + text 
		 + ESC + "39m" + ESC + "49m"
}

// produces an ansi sequence for a certain colour either foreground or background
ansiEscapes.colourSeq = (col, isFG) => {
	// get colours with 0 in defaults
	isFG ??= false;
	let c = {...(isFG ? ansiEscapes.FGCol : ansiEscapes.BGCol), ...col}
	// return text string
	return ESC + `${isFG ? '38' : '48'};2` + SEP + c.r + SEP + c.g + SEP + c.b + "m"
}


// A class for colours
ansiEscapes.Colour = class Colour {
	// a set of predefined static colours
	static Red = new Colour({r: 255});
	static Blue = new Colour({b : 255});
	static Green = new Colour({g : 255});
	static Yellow = new Colour({r:236,g:232,b:26});
	static DarkGrey = new Colour({r:50, g:50, b:50});
	static LtDkGrey = new Colour({r:75, g:75, b:75});
	static Grey = new Colour({r:128, g:128, b:128});
	static LightGrey = new Colour({r:190, g:190, b:190});
	static White = new Colour({r:255, g:255, b:255});

	// rounds and restricts a value to the colour range (0, 255)
	static restrict = (val) => Math.round(clamp(0, val, 255));

	// simple constructor with optional colours
	constructor({r, g, b}) {
		this.r = r ?? 0;
		this.g = g ?? 0;
		this.b = b ?? 0;
	}


	// multiplies the colour with the given multiplier, clamping to range.
	mul(multiplier) {
		return new Colour({
			r: Colour.restrict(this.r * multiplier),
			g: Colour.restrict(this.g * multiplier),
			b: Colour.restrict(this.b * multiplier)
		})
	}

	// blends two colours linearly using lerp to transition
	blend(otherCol, lerp) {
		const mix = (c) => this[c]*(1 - lerp) + otherCol[c]*lerp
		return new Colour({
			r: Colour.restrict(mix('r'), 255),
			g: Colour.restrict(mix('g'), 255), 
			b: Colour.restrict(mix('b'), 255) 
		})
	}
}

export default ansiEscapes;
