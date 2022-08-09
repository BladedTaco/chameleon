import ESC from "./ansiEscapes";
import wrasse from "./wrasse";
import {sleep, clamp, within, group_n} from './util';

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

        Window.setup();
    }

    // statics
    static drawReq = {
        request: false,
        setup : false,
        callbacks : [],
    };

    onWheel(event) {
        if (!this.scrollable) return false;
        if (!this.mouseWithin(event.offsetX, event.offsetY)) return false;

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
        this.requestDraw();
        return true;
    }

    static setup() {
        if (Window.drawReq.setup) return;
        Window.drawReq.setup = true;

        // setup scrolling
        wrasse.html.terminal.addEventListener('wheel', (event) => {
            for (let i = wrasse.perm.windows.length - 1; i >= 0; i--) {
                if (wrasse.perm.windows[i].onWheel(event)) return;
            }
            wrasse.window.onWheel(event);
        }, {
            capture : true,
            passive : false,
        });

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

    /*
        ESC.cursorSavePosition
        ESC.cursorRestorePosition
        ESC.deleteLine
        ESC.cusorTo
        ESC.insertLine
    */

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
        const cellHeight = wrasse.html.terminal.offsetHeight / wrasse.terminal.rows;
        const cellWidth  = wrasse.html.terminal.offsetWidth  / wrasse.terminal.cols;
        return within(this.x - 0.5, relX / cellWidth  - 1, this.x + this.width  + 0.5)
            && within(this.y - 0.5, relY / cellHeight - 1, this.y + this.height + 0.5);
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