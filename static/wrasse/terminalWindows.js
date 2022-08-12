import ESC from "./ansiEscapes";
import wrasse from "./wrasse";
import {sleep, clamp, within, group_n, null_func} from './util';

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
        this.funcs = {
            enter : null_func,
            leave : null_func,
            click : null_func,
            ...funcs
        }
        this.colour = colour ?? ESC.Colour.Red;

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
        this.content = [""];
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
        Window.setupEvent('click', 'onMouseClick');

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
        this.onMouseMove();
        this.requestDraw();
        return true;
    }

    onMouseMove(event) {
        this.active = this.mouseWithin(event.offsetX, event.offsetY);
        // exit if mouse not within
        if (!this.active) return false;

        // get cell x and y
        const {x, y} = this.mouseToCell(event.offsetX, event.offsetY);

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
                    ? x.funcs.leave()
                    : x.funcs.enter();
                x.active = !x.active;
            });

        this.requestDraw();
        return true;
    }

    onClick(event) {
        // exit if mouse not within
        if (!this.active) return false;

        // get active links
        this.links
            .filter(x => x.active)
            .forEach(x => x.funcs.click());

        this.requestDraw();
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
        this.links.push(new Link(
            window, range, funcs, colour
        ));
    }

    reset() {
        this.clean();
        this.content = [""];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        this.line = 0;
    }

    write(content, callback) {
        const handleEscape = (content) => {
            // special character regex
            const regex = /\u001B\[(?:(?<nums>(?:[0-9]+;)*)(?<num>(?:[0-9]+)))?(?<char>[a-zA-Z])/gi;

            // handle special characters
            let cutString = [];
            let anyMatches = false;
            for (const [prefix, _nums, num, char] of group_n(content.split(regex), 4)) {
                let cut = {prefix, esc: ()=>{}};
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
                            this.content.splice(this.cursor.y+1, 0, ...Array(+nums[0]).fill(""));
                        }
                    break;
                    // Delete Line (current line delted)
                    case 'M':
                        cut.esc = () => {
                            this.content.splice(this.cursor.y, +num);
                        }
                    break;
                    // Fallthrough
                    default:
                        console.log(nums)
                        console.log(`UNMATCHED ESCAPE SEQUENCE: ESC[${nums.join(";") + char}`)
                    break;
                }
                cutString.push(cut);
            }
            if (anyMatches == false) return [{prefix: content, esc: ()=>{}}];

            return cutString;
        };

        // write the string to the window
        console.log({content})
        for (const {prefix, esc} of handleEscape(content)) {
            console.log({prefix})
            let oldCursor = {...this.cursor};
            for (const line of prefix.split('\n')) {
                console.log({line})
                // ensure cursor isn't off content bounds
                while (this.cursor.y >= this.content.length) {
                    this.content.push("");
                }
                // overwrite content with line
                this.content[this.cursor.y] = this.content[this.cursor.y]
                    .padEnd(this.cursor.x)
                    .splice(this.cursor.x, line.length, line);
                // intercalate cursor reset
                this.cursor.x += line.length;
                oldCursor = {...this.cursor};
                this.cursor.y += 1
                this.cursor.x = 0;
            }
            // restore last oldCursor
            this.cursor = oldCursor;
            // handle escape characters
            esc();
            // ensure cursor isn't off content bounds
            while (this.cursor.y >= this.content.length) {
                this.content.push("");
            }
        }

        this.requestDraw(callback);
        console.log(JSON.parse(JSON.stringify({content:this.content, cursor:this.cursor})));
    }

    writeln(content, callback) {
        return this.write(content + "\n", callback);
    }

    mouseWithin(relX, relY) {
        const {x, y} = mouseToCell(relX, relY);
        return within(this.x - 0.5, x, this.x + this.width  + 0.5)
            && within(this.y - 0.5, y, this.y + this.height + 0.5);
    }

    mouseToCell(relX, relY) {
        const cellHeight = wrasse.html.terminal.offsetHeight / wrasse.terminal.rows;
        const cellWidth  = wrasse.html.terminal.offsetWidth  / wrasse.terminal.cols;
        return {
            x : relX / cellWidth  - 1,
            y : relY / cellHeight - 1,
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
            yield line.slice(0, this.width);
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
            writeString += `${ESC.cursorTo(this.x, this.y + i)}║${(lines.next().value || "").padEnd(this.width)}║`;
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