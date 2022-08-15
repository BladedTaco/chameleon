import ESC from "./ansiEscapes";
import wrasse from "./wrasse";
import {sleep, clamp, within, group_n, null_func, deep_copy} from './util';

String.prototype.splice = function (index, count, add) {
    // We cannot pass negative indexes directly to the 2nd slicing operation.
    if (index < 0) {
      index = this.length + index;
      if (index < 0) {
        index = 0;
      }
    }
  
    return this.slice(0, index) + (add || "") + this.slice(index + count);
}

class Link {
    constructor(window, range, funcs, colour) {
        this.window = window;
        this.range = {
            start : {x : 0, y : 0, ...range.start},
            end : {x : 0, y : 0, ...range.end},
        }
        this.colour = {r:0, g:0, b:0, ...(colour ?? ESC.Colour.Red)}
        this.funcs = {
            enter : (link) => {
                link.window.write(
                    ESC.cursorSavePosition +
                    ESC.cursorTo(link.range.start.x, link.range.start.y) + 
                    ESC.colouredText({}, link.colour, "|")
                        .split("|")
                        .join(ESC.cursorTo(link.range.end.x, link.range.end.y)) +
                    ESC.cursorRestorePosition
                )
                funcs?.enter?.(link);
            },
            leave : (link) => {
                link.window.content[link.range.start.y].esc = []
                funcs?.leave?.(link);
            },
            click : (link) => funcs?.click?.(link)
        }

        this.active = false;
    }
}

class Window {
    constructor(terminal, x, y, width, height, options) {
        this.terminal = terminal;
        this.x = Math.ceil(x);
        this.y = Math.ceil(y);
        this.width = Math.floor(width);
        this.height = Math.floor(height);
        this.line = 0;
        this.content = [{text:"", esc:[]}];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        options = {movable : false, scrollable : true, resizable : false, ...options};
        this.movable = options.movable;
        this.scrollable = options.scrollable;
        this.resizable = options.resizable;
        this.links = [];
        this.active = false;

        Window.setup();
    }

    // statics
    static drawReq = {
        request: false,
        setup : false,
        callbacks : [],
    };

    static setup() {
        if (Window.drawReq.setup) return;
        Window.drawReq.setup = true;

        // setup events
        Window.setupEvent('wheel', 'onWheel');
        Window.setupEvent('mousemove', 'onMouseMove');
        Window.setupEvent('click', 'onClick');

        // setup draw requests
        (async () => {
            while(true) {
                if (Window.drawReq.request) {
                    wrasse.window.draw();
                    wrasse.perm.windows.forEach(x => x.draw());
                    Window.drawReq.request = false;
                    Window.drawReq.callbacks.forEach(x => x());
                    Window.drawReq.callbacks = [];
                }
                await sleep(1);
            }
        })();
    }

    static setupEvent(eventType, eventFunc) {
        wrasse.html.terminal.addEventListener(eventType, (event) => {
            for (let i = wrasse.perm.windows.length - 1; i >= 0; i--) {
                if (wrasse.perm.windows[i][eventFunc](event)) return;
            }
            wrasse.window[eventFunc](event);
        }, {
            capture : true,
            passive : false,
        });
    }


    onWheel(event) {
        if (!this.scrollable || !this.active) return false;

        const dir = Math.sign(event.deltaY);
        if (event.shiftKey) {
            this.move(dir, 0, true);
        } else if (event.altKey) {
            this.move(0, dir, true);
        } else {
            this.line = clamp(0, 
                this.line + dir,
                this.content.length - this.height
            );
        }
        this.onMouseMove(event);
        this.requestDraw();
        return true;
    }

    onMouseMove(event) {
        this.active = this.mouseWithin(event.offsetX, event.offsetY);
        // exit if mouse not within
        if (!this.active) return false;

        // get cell x and y
        let {x, y} = this.mouseToCell(event.offsetX, event.offsetY);
        y += this.line 
        this.links
            // get links changing state
            .filter(curr => (
                    within(curr.range.start.x, x, curr.range.end.x)
                    && within(curr.range.start.y, y, curr.range.end.y)
                ) != curr.active
            )
            // perform enter/exit functions and flip state
            .forEach(x => {
                x.active 
                    ? x.funcs.leave(x)
                    : x.funcs.enter(x);
                x.active = !x.active;
                this.requestDraw();
            });

        // this.requestDraw();
        return true;
    }

    onClick(event) {
        // exit if mouse not within
        if (!this.active) return false;

        // get active links
        this.links
            .filter(x => x.active)
            .forEach(x => x.funcs.click(x));

        // this.requestDraw();
        return true;
    }

    /*
        ESC.cursorSavePosition
        ESC.cursorRestorePosition
        ESC.deleteLine
        ESC.cusorTo
        ESC.insertLine
    */

    addLink(range, funcs, colour) {
        const window = this;
        const link = new Link(
            window, range, funcs, colour
        )
        this.links.push(link);
        if (colour != ESC.Colour.Blue) {
            console.log({link})
        }
    }

    reset() {
        this.clean();
        this.content = [{text:"", esc:[]}];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        this.line = 0;

        return this;
    }

    write(text, callback) {
        const handleEscape = (text) => {
            // special character regex
            const regex = /\u001B\[(?:(?<nums>(?:[0-9]+;)*)(?<num>(?:[0-9]+)))?(?<char>[a-zA-Z])/gi;

            // handle special characters
            let cutString = [];
            let anyMatches = false;
            for (const [prefix, _nums, num, char] of group_n(text.split(regex), 4)) {
                let cut = {prefix, esc: null_func};
                if ((_nums || num || char) === undefined) {
                    cutString.push(cut)
                    break;
                }
                anyMatches = true;
                let nums = (_nums?.split(';') || [])
                    .filter(x => x !== '');
                nums.push(num);
                switch (char) {
                    // // Colours
                    // case 'm':
                    //     cut.esc = () => {
                            
                    //     }
                    // break;
                    // cursorTo
                    case 'G':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, x: num+1};
                        }
                    break;
                    case 'H':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, x: +nums[1]-1, y: +nums[0]-1};
                        }
                    break;
                    // Save Cursor
                    case 's':
                        cut.esc = () => {
                            this.cursor.saved = {x: this.cursor.x, y: this.cursor.y};
                        }
                    break;
                    // Restore Cursor
                    case 'u':
                        cut.esc = () => {
                            this.cursor = {...this.cursor, ...this.cursor.saved};
                        }
                    break;
                    // Insert Line (current line pushed up)
                    case 'L':
                        cut.esc = () => {
                            this.content.splice(
                                this.cursor.y+1,
                                0, 
                                ...Array.from({length: +nums[0]}, _ => ({text:"", esc:[]}))
                            );
                        }
                    break;
                    // Delete Line (current line delted)
                    case 'M':
                        cut.esc = () => {
                            this.content.splice(this.cursor.y, +num);
                        }
                    break;
                    // Colours
                    case 'm':
                        cut.esc = () => `\u001B[${nums.join(";") + char}`
                    break;
                    // Fallthrough
                    default:
                        console.log(`UNMATCHED ESCAPE SEQUENCE: ESC[${nums.join(";") + char}`)
                        cut.esc = () => `\u001B[${nums.join(";") + char}`
                    break;
                }
                cutString.push(cut);
            }
            if (anyMatches == false) return [{prefix: text, esc: null_func}];

            return cutString;
        };

        // write the string to the window
        for (const {prefix, esc} of handleEscape(text)) {
            let oldCursor = {...this.cursor};
            for (const line of (prefix ?? "").split('\n')) {
                // ensure cursor isn't off content bounds
                while (this.cursor.y >= this.content.length) {
                    this.content.push({text:"", esc:[]});
                }
                // overwrite content with line
                this.content[this.cursor.y].text = this.content[this.cursor.y].text
                    .padEnd(this.cursor.x)
                    .splice(this.cursor.x, line.length, line);
                // overwrite escape sequences too
                this.content[this.cursor.y].esc = this.content[this.cursor.y].esc
                    .filter(({pos}) => !within(1, pos - this.cursor.x, line.length - 2))
                // intercalate cursor reset
                this.cursor.x += line.length;
                oldCursor = {...this.cursor};
                this.cursor.y += 1
                this.cursor.x = 0;
            }
            // restore last oldCursor
            this.cursor = oldCursor;
            // handle escape characters
            let add = esc();
            if (add) {
                this.content[this.cursor.y].esc.push({pos : this.cursor.x, seq : add})
            }
            // ensure cursor isn't off content bounds
            while (this.cursor.y >= this.content.length) {
                this.content.push({text:"", esc:[]});
            }
        }

        this.requestDraw(callback);
    }

    writeln(text, callback) {
        return this.write(text + "\n", callback);
    }

    mouseWithin(relX, relY) {
        const {x, y} = this.mouseToCell(relX, relY);
        return within(this.x - 0.5, x, this.x + this.width  + 0.5)
            && within(this.y - 0.5, y, this.y + this.height + 0.5);
    }

    mouseToCell(relX, relY) {
        const cellHeight = wrasse.html.terminal.offsetHeight / wrasse.terminal.rows;
        const cellWidth  = wrasse.html.terminal.offsetWidth  / wrasse.terminal.cols;
        return {
            x : Math.floor(relX / cellWidth  - 1),
            y : Math.floor(relY / cellHeight - 1),
        }
    }

    resize(width, height, relative) {
        if (!this.resizable) return;

        // check for relative resizing
        if (relative) {
            width += this.width;
            height += this.height;
        }

        // clean old position
        this.clean();

        // update size
        this.width = width;
        this.height = height;
        this.requestDraw();
    }

    expand() {
        this.move(0, 0, false);
        this.resize(this.terminal.cols - 2, this.terminal.rows - 2);
    }

    move(x, y, relative) {
        if (!this.movable) return;

        // check for relative positioning
        if (relative) {
            x += this.x;
            y += this.y;
        }

        x = clamp(0, x, this.terminal.cols - this.width - 2)
        y = clamp(0, y, this.terminal.rows - this.height - 2)

        // clean old position
        this.clean();

        // update position
        this.x = x;
        this.y = y;
        this.requestDraw();
    }

    *lines() {
        for (const line of this.content.slice(this.line, this.line + this.height)) {
            yield {
                text: line.text.slice(0, this.width),
                esc: line.esc.filter(({pos}) => within(0, pos, this.width))
            }
        }
    }

    
    draw() {
        // draw top and bottom border
        let writeString =
            ESC.cursorSavePosition 
          + ESC.cursorTo(this.x, this.y) 
          + `╔${"═".repeat(this.width)}╗`
          + ESC.cursorTo(this.x, this.y + this.height + 1) 
          + `╚${"═".repeat(this.width)}╝`;
        
        // draw content
        let lines = this.lines();
        for (let i = 1; i <= this.height; i++) {
            // get the next line
            const {text, esc} = lines.next().value ?? {text:"", esc:[]};
        
            // add it to the write string
            writeString += `${ESC.cursorTo(this.x, this.y + i)}║${
                // sort escape sequences by position
                esc.sort((a, b) => a.pos - b.pos)
                // reduce from highest position to lowest
                .reduceRight(
                    // insert the escape sequence into the string
                    (acc, {pos, seq}) => acc.splice(pos, 0, seq)
                    , (text || "").padEnd(this.width)
                )
            }║`;
        }
        // restore cursor
        this.terminal.write(writeString + ESC.cursorRestorePosition);
    }
    
    requestDraw(callback) {
        // request draw
        Window.drawReq.request = true;
        if (callback) Window.drawReq.callbacks.push(callback);
    }

    clean() {
        // save cursor
        this.terminal.write(ESC.cursorSavePosition )
            
        // draw clear
        for (let i = 0; i <= this.height+1; i++) {
            this.terminal.write(
                `${ESC.cursorTo(this.x, this.y + i)}${"".padEnd(this.width + 2)}`
            );
        }

        // restore cursor
        this.terminal.write(ESC.cursorRestorePosition);
    }

    
}



const termWindows = {
    "Window" : Window,
};

export default termWindows;