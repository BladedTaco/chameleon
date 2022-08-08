import ESC from "./ansiEscapes";
import wrasse from "./wrasse";

const clamp = (min, num, max) => Math.min(Math.max(num, min), max);
const within = (min, num, max) => min <= num && num <= max;

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
    constructor(terminal, x, y, width, height, options = {movable: false, scrollable: true, resizable: false}) {
        this.terminal = terminal;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.line = 0;
        this.content = [];
        this.cursor = {x:0, y:0, saved: {x:0, y:0}};
        this.movable = options.movable;
        this.scrollable = options.scrollable;
        this.resizable = options.resizable;


        wrasse.html.terminal.addEventListener('wheel', (event) => {
            if (!this.scrollable) return;
            if (!this.mouseWithin(event.offsetX, event.offsetY)) return;

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
            this.draw();
        }, {
            capture : true,
            passive : false,
        });

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
        this.content = [];
    }

    write(content, callback) {
        const handleEscape = (content) => {
            // special character regex
            const regex = /\u001B\[(?<nums>([0-9]+;)*)?(?<num>([0-9]+))(?<char>[a-zA-Z])/g;

            // handle special characters
            let cutString = [];
            let lastIndex = 0;
            let anyMatches = false;
            for (const match of content.matchAll(regex)) {
                anyMatches = true;
                let {nums, num, char} = match.groups;
                let cut = {prefix: match.input.slice(lastIndex, match.index), esc: ()=>{}};
                lastIndex = match.index + match[0].length;
                nums = nums?.split(';') || [];
                nums.filter(x => x === '');
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
                            this.cursor = {...this.cursor, x: nums[0]+1, y: nums[1]+1};
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
                    // Insert Line
                    case 'L':
                        cut.esc = () => {
                            this.content.splice(this.cursor.y, 0, Array(num).fill(""));
                        }
                    break;
                    // Delete Line
                    case 'M':
                        cut.esc = () => {
                            this.content.splice(this.cursor.y, num);
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
        for (const {prefix, esc} of handleEscape(content)) {
            let oldCursor = {...this.cursor};
            for (const line of prefix.split('\n')) {
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

        this.draw();
        console.log(this)

        this.terminal.write("", callback);
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
        this.draw();
    }

    expand() {
        this.resize(this.terminal.cols, this.terminal.rows);
    }

    move(x, y, relative) {
        if (!this.movable) return;

        // check for relative positioning
        if (relative) {
            x += this.x;
            y += this.y;
        }

        x = clamp(0, x, this.terminal.cols)
        y = clamp(0, y, this.terminal.rows)

        // clean old position
        this.clean();

        // update position
        this.x = x;
        this.y = y;
        this.draw();
    }

    *lines() {
        for (const line of this.content.slice(this.line, this.line + this.height)) {
            yield line.slice(0, this.width);
        }
    }

    
    draw() {
        // draw top and bottom border
        this.terminal.write(
            ESC.cursorSavePosition 
          + ESC.cursorTo(this.x, this.y) 
          + `╔${"═".repeat(this.width)}╗`
          + ESC.cursorTo(this.x, this.y + this.height + 1) 
          + `╚${"═".repeat(this.width)}╝`
        )
            
        // draw content
        let lines = this.lines();
        for (let i = 1; i <= this.height; i++) {
            this.terminal.write(
                `${ESC.cursorTo(this.x, this.y + i)}║${(lines.next().value || "").padEnd(this.width)}║`
            );
        }

        // restore cursor
        this.terminal.write(ESC.cursorRestorePosition);
    }
    
    clean() {
        // save cursor
        this.terminal.write(ESC.cursorSavePosition )
            
        // draw content
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