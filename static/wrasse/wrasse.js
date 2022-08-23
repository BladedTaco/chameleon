import { Terminal as xTerminal } from 'xterm'
import { FitAddon } from 'xterm-addon-fit';
import ESC from './ansiEscapes';
import wrasseGHC from './wrasseGHC';
import messages from './messages';
import tWin from './terminalWindows' ;
import {sleep, clamp, within, start_pattern_gen, null_func} from './util';

let initialized = false;
const WrasseTerminal = new xTerminal({
    convertEol: true,
    scrollback: 0,
    cursorBlink: false,
    cursorStyle: "bar",
    cursorWidth: 1,
    disableStdin: true,
    tabStopWidth: 2,
    //font
    //windowsMode
});
const fitAddon = new FitAddon();
WrasseTerminal.loadAddon(fitAddon);

const html = {
    terminal : document.getElementById('terminal-container'),
    buttons : [
      document.getElementById('wrasse_0'),
      document.getElementById('wrasse_1'), 
      document.getElementById('wrasse_2'), 
      document.getElementById('wrasse_3'), 
      document.getElementById("wrasse_tree")
    ],
    hover : {
      shell : document.getElementById("wrasse-hover"),
      content : document.getElementById("wrasse-hover-content"),
    },
    block: document.getElementById("wrasse-block"),
}




let scrollToTop = () => {
  wrasse.window.line = 0;
  wrasse.window.requestDraw();
}

const fitTerminal = () => {
  fitAddon.fit();
  wrasse.window.resizable = true;
  wrasse.window.movable = true;
  wrasse.window.expand();
  wrasse.window.movable = false;
  wrasse.window.resizable = false;
}

let wrasse_setup = () => {
    console.log('wrasse init')

    console.log(wrasse.editor)

    wrasse.terminal.open(html.terminal);

    wrasse.window = new tWin.Window(WrasseTerminal, 2, 2, 10, 10);

    initialized = true;

    // ignore all keypresses
    // wrasse.terminal.attachCustomKeyEventHandler((_) => false);
    wrasse.terminal.modes.mouseTrackingMode = "any"
    wrasse.terminal.modes.wraparoundMode = true;

    html.buttons[0].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_0)
    });

    html.buttons[1].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_1)
    });

    html.buttons[2].addEventListener('click', _ => {
        wrasse.switch_terminal(wrasse.data_2)
        let win = new tWin.Window(wrasse.terminal, 5, 5, 10, 5, {movable : true, scrollable : true});
        win.content = [
          "1234567",
          " - ", 
          "23432423423423",
          "sdfdsfdf",
          "",
          ",",
          "sdfd",
          "34"
        ]
        win.draw();
        perm.windows.push(win);
    });

    html.buttons[3].addEventListener('click', _ => {
        fitTerminal();
    });

    html.buttons[4].addEventListener('click', _ => {
      wrasse.interactive_terminal(wrasse.tree)
    });

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitTerminal();
    // wrasse.terminal.resize(wrasse.terminal.cols, 1000);

    // write starter text
    // wrasse.window.write('Hello from \x1B[1;3;31mxterm.js\x1B[0m $ ', scrollToTop)
    wrasse.window.write(`Hello from ${ESC.colouredText(ESC.Colour.Red, ESC.Colour.Blue, "xterm.js")}m \n$ `, scrollToTop)

    // setup mouse followers
    const onMouseMove = (e) =>{
      html.hover.shell.style.left = e.pageX + 'px';
      html.hover.shell.style.top = e.pageY + 'px';

      let off = 0;
      if (html.block.classList.contains("hidden")) {
        off = 100;
      }

      html.block.style.left = (e.pageX - off) + 'px';
      html.block.style.top = (e.pageY - off) + 'px';
    }

    document.addEventListener('mousemove', onMouseMove);

    // load messages
    (async () => {
      let response = await fetch('/messages', {
        method: 'POST',
        body: 'messages',
      });
      let data = response.json();

      console.log(data)

      wrasse.messages = await data;
    })();
}

let hook = async ({code, response, editor}) => {
    let data = await response;

    wrasse.editor = editor;
    console.log('back-end call')
    if (!initialized) {
        wrasse.setup()
    } 

    // send code to ghc handler
    let ghc_data = await handle_ghc(code);

    console.log(ghc_data)

    wrasse.tree = parse_tree(ghc_data.full)
    wrasse.data_0 = ghc_data
    wrasse.data_0.ghc.code = code.split("\n")
    wrasse.data_1 = data
    wrasse.data_2 = { ghc: ghc_data, chameleon: data }


    console.log(wrasse.tree)
    // switch_terminal(wrasse.data_0)

    interactive_terminal(wrasse.tree)
}

let parse_tree = (tree) => {
  return {
    content: tree[0][0],
    active: tree[0][1],
    line: tree[0][2],
    children: tree[1].map(parse_tree),
    link: undefined,
  }
}


let switch_terminal = (data) => {
  // clean disposables
    perm.disposables.forEach(x => x.dispose())
    perm.disposables = [];
    perm.keywords.forEach(x => x.dispose())
    perm.keywords = [];
    perm.windows = [];

    wrasse.terminal.reset()
    wrasse.window.reset();
    wrasse.terminal.options.disableStdin = true;
    if (data) {
      wrasse.window.writeln(JSON.stringify(
          data,
          null,
          2
      ), scrollToTop)
    }

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitTerminal();

    // star drawing
    let starGen = (function *() {
      while (true) {
        for (const item of ["|", "/", "-", "\\"]) {
          yield item;
       }
      }
    })();

    let numGen = (function *() {
      let i = 0
      while (true) {
        yield ++i;
      }
    })();
    
    let recurse = async () => {
      sleep(100);
      wrasse.window.write(ESC.cursorSavePosition 
        + ESC.cursorTo((numGen.next().value % 2) + 30, 1) + starGen.next().value
        + ESC.cursorRestorePosition
        , recurse)
    }
    // recurse();
}


const set_hover_content = (text) => {
  // hide/show based on if text is provided
  if (typeof text === 'undefined' || text === "") {
    // hide element
    // html.hover.shell.classList.add("hidden")
  } else { 
    // update text and show element
    // html.hover.content.innerText = text
    // html.hover.shell.classList.remove("hidden")
    perm.windows[0].reset()
      .writeln(text || "");
  }
}

const block_mouse = (bool) => {
  if (!bool) {
    // hide element
    html.block.classList.add("hidden")
  } else { 
    // show element
    html.block.classList.remove("hidden")
  }
}


let interactive_terminal = (tree) => {
  // clear terminal
  // wrasse.terminal.reset()
  switch_terminal();

  // let parse_tree = (tree) => {
  //   return {
  //     content: tree[0][0],
  //     active: tree[0][1],
  //     line: tree[0][2],
  //     children: tree[1].map(parse_tree)
  //   }
  // }
  
  let hoverWin = new tWin.Window(
    wrasse.terminal,
    wrasse.terminal.cols / 2,
    0,
    wrasse.terminal.cols / 2 - 2,
    wrasse.terminal.rows / 2,
    {movable : true}
  );
  perm.windows.push(hoverWin);

  let curr_line = 0;

  const write_text = (node, level, write = false) => {
    let line = curr_line

    let prefix = "  ".repeat(level) 
    let node_string = node.content //JSON.stringify(node)// node.content
    // wrasse.window.writeln(ESC.colouredText({r:100, g:200, b:100}, {}, node_string));

    // write tree string
    if (write) {
      if (node.children.length == 0) {
        wrasse.window.writeln(prefix + "- " + node_string);
      } else {
        if (node.active) {
          wrasse.window.writeln(prefix + "▼ " + node_string);
        } else {
          wrasse.window.writeln(prefix + "► " + node_string);
        }
      }
    }

    node.line = line;
    
    curr_line++;

    // recurse if active
    if (node.active) {
      node.children.forEach(x => write_text(x, level+1, write))
    }
  }

  const clean_lines = (node, level, mode) => {

    // first call
    if (mode === undefined) {
      // set mode
      mode = node.active;
    // else exit on inactive node
    } else if (node.active == false) return;

    if (mode) {
      // delete child lines
      node.children.slice().reverse().forEach(x => clean_lines(x, level+1, mode))

      // delete own lines.
      wrasse.window.write(
        ESC.cursorSavePosition +
        ESC.cursorTo(0, node.line+1) +
        ESC.deleteLine(node.children.length) +
        ESC.cursorTo(2*level, node.line) + "►" +
        ESC.cursorRestorePosition
      )
    } else {
      ///write in new lines
      
      // insert childnum empty lines
      wrasse.window.write(
        ESC.cursorSavePosition +
        ESC.cursorTo(0, node.line) +
        ESC.insertLine(node.children.length)
      )

      let curr_line = node.line+1;
      // write each child line
      node.children.forEach(x => {
        let prefix = "  ".repeat(level+1)
        if (x.children.length == 0) {
          wrasse.window.write(
            ESC.cursorTo(0, curr_line) +
            prefix + "- " + x.content
          );
        } else {
          wrasse.window.write(
            ESC.cursorTo(0, curr_line) +
            prefix + "► " + x.content
          );
        }
        curr_line++;
      });

      // restore cursor
      wrasse.window.write(
        ESC.cursorTo(2*level, node.line) + "▼" +
        ESC.cursorRestorePosition
      )

      // write child lines
      node.children.forEach(x => clean_lines(x, level+1, mode))
    }
  }

  const register_links = (node, level, ignoreLine) => {
    // register link provider
    node.link = {
      text: node.content,
      range : { 
        start: { x: level*2+ 1,                        y: node.line },
        end:   { x: level*2 + 2 + node.content.length, y: node.line } 
      },
    }
    if (node.children.length > 0 && node.line != ignoreLine) {
      let hovered = false;
      wrasse.window.addLink(
        node.link.range,
        {
          click(link) {
            // remove all link providers except this one
            link.window.links = [link]

            // fix terminal output
            clean_lines(node, level)

            // flip state
            node.active = !node.active;

            // update tree state
            curr_line = 0
            write_text(tree, 0, false)
            register_links(tree, 0, node.line);

            perm.keywords.forEach(x => x.dispose())
            perm.keywords = [];
            register_keywords(tree)

            wrasse.window.write(ESC.cursorTo(0, node.line));
          },
          enter(link) {
            wrasse.set_hover_content(node.active
              ? "click to collapse"
              : "click to expand")
          },
          leave(link) {
            hovered = false;
            wrasse.set_hover_content()
          }
        },
        ESC.Colour.Blue
      );
    }

    if (node.active) {
      node.children.forEach(x => register_links(x, level+1))
    }
  }
  
  const register_keywords = (node) => {
    if (node?.link) {
      const {text, range} = node.link;
      // ambiguous
      for (const match of text.matchAll(wrasseGHC.regex.ambiguous)) {
        const {namespace, symbol} = match.groups;
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 3,                y: node.line },
            end:   { x: range.start.x + match.index + match[0].length,  y: node.line } 
          },
          {
            click(link) {
              if (node.active) {
                node.children = node.children.pop();
                return;
              }

              // star drawing
              let starGen = start_pattern_gen();
              let finished = false;

              let recurse = () => {
                sleep(10).then(() => {
                  if (finished) return;

                  wrasse.window.write(ESC.cursorSavePosition 
                    + ESC.cursorTo(range.start.x - 1, node.line) + starGen.next().value
                    + ESC.cursorRestorePosition
                    , recurse)
                })
              }
              recurse();

              (async () => {
                console.log(wrasse)

                let code = wrasse.data_0?.ghc.code
                .map((x) => {
                  let arr = x.split('=');
                  if (arr.length <= 1) {
                    return x;
                  }
                  let new_x = arr.slice(1).join("=");
                  return arr[0] + "=" + new_x.replace(symbol, `${namespace}${symbol}`);
                })
                .reduce((acc, curr) => {
                  return acc + '\n' + curr;
                })

                console.log(code)

                let response = await fetch('/ghc', {
                  method: 'POST',
                  body: code,
                });
                let data = await response.json();


                console.log(data)
                const add = parse_tree(data.full);
                const cd = add.children
                  .find(x => x.content === "GHC")
                  ?.children.find(x => x.content === "code") 
                  ?? {};
                cd.content = "code [[commit]]"
                cd.code = code
                node.children.push(add);

                finished = true;

                const [l, s] = [wrasse.window.line, wrasse.window.scroll];

                wrasse.interactive_terminal(wrasse.tree);

                wrasse.window.line = l;
                wrasse.window.scroll = s;
              })();
            },
            enter(link) {
              wrasse.set_hover_content(`See what happens if you use '${symbol}'`)
            },
            leave(link) {
              wrasse.set_hover_content()
            }
          },
          ESC.Colour.Green
        );
      }

      // keywords
      for (const match of text.matchAll(wrasseGHC.regex.keyword)) {
        wrasse.window.addLink(
            { 
              start: { x: range.start.x + match.index + 2,                    y: node.line },
              end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line } 
            },
            {
              click(link) {
              },
              enter(link) {
                wrasse.set_hover_content(wrasseGHC.map[match[0]])
              },
              leave(link) {
                wrasse.set_hover_content()
              }
            },
            ESC.Colour.Grey
          );
      }

      // code commit
      for (const match of text.matchAll(wrasseGHC.regex.codeCommit)) {
        wrasse.window.addLink(
            { 
              start: { x: range.start.x + match.index + 2,                    y: node.line },
              end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line } 
            },
            {
              click(link) {
                wrasse.editor(node?.code)
              },
              enter(link) {
                wrasse.set_hover_content("Commit code to main window")
              },
              leave(link) {
                wrasse.set_hover_content()
              }
            },
            ESC.Colour.Grey
          );
      }

      // symbols
      for (const match of text.matchAll(wrasseGHC.regex.symbol)) {
        wrasse.window.addLink(
            { 
              start: { x: range.start.x + match.index + 2,                y: node.line },
              end:   { x: range.start.x + match.index + match[0].length,  y: node.line } 
            },
            {
              click(link) {
              },
              enter(link) {
                const {symbol} = match.groups;
                

                wrasse.set_hover_content(`${symbol}
                
                type: TODO
                defined at: TODO
                etc.: TODO`)
              },
              leave(link) {
                wrasse.set_hover_content()
              }
            },
            ESC.Colour.LightGrey
          );
      }

      // code locations
      for (const match of text.matchAll(wrasseGHC.regex.location)) {
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 1,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line }
          },
          {
            click(link) {
            },
            enter(link) {
              const {line, colStart, colEnd} = match.groups;
              
              let x = ""
              if (wrasse?.data_0?.ghc?.code) {
                x = wrasse?.data_0?.ghc?.code[line-1];
              }

              wrasse.set_hover_content(`not implemented, look at line ${line}, column ${colStart} to ${colEnd}
              ${x}`)
            },
            leave(link) {
              wrasse.set_hover_content()
            }
          } 
        );
      }

      // Error code
      for (const match of text.matchAll(wrasseGHC.regex.error)) {
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 2,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length,  y: node.line }
          },
          {
            click(link) {
            },
            enter(link) {
              const {code} = match.groups;
              let msg = wrasse.messages.find((x) => x.errCode == code);

              if (msg) {
                wrasse.set_hover_content(JSON.stringify(msg, null, 2))
              } else {
                wrasse.set_hover_content(`No file found for error code ${code}`)
              }
            },
            leave(link) {
              wrasse.set_hover_content()
            }
          } 
        );
      }

    }
    node?.children.forEach(register_keywords)
  }

  write_text(tree, 0, true);

  register_links(tree, 0)

  register_keywords(tree)
}


let handle_ghc = async (code) => {
    return ghc_hook(code)
}

let ghc_hook = async (code) => {
    let response = await fetch('/ghc', {
        method: 'POST',
        body: code,
    });
    return response.json();
}


const perm = {
  "disposables": [],
  "keywords": [],
  "windows": [],
}


const wrasse = {
    "html" : html,
    "perm" : perm,
    "hook": hook,
    "setup": wrasse_setup,
    "terminal": WrasseTerminal,
    "window" : undefined,
    "tree" : {},
    "data_0" : {},
    "data_1" : {},
    "data_2" : {},
    "switch_terminal" : switch_terminal,
    "interactive_terminal" : interactive_terminal,
    "set_hover_content" : set_hover_content,
    "block_mouse" : block_mouse,
    "messages" : [],
    "editor" : null_func,
};

/*
underlined on hover
\x1b[3mVS Code\x1b[0m

https://xtermjs.org/


decorations
function addDecoration(term) {
  const marker = term.addMarker(15);
  const decoration = term.registerDecoration({ marker, x: 44 });
  decoration.onRender(element => {
    element.classList.add('link-hint-decoration');
    element.innerText = 'Try clicking italic text';
    // must be inlined to override inlined width/height coming from xterm
    element.style.height = '';
    element.style.width = '';
  });
}



data input

    term.onData(e => {
      switch (e) {
        case '\u0003': // Ctrl+C
          term.write('^C');
          prompt(term);
          break;
        case '\r': // Enter
          runCommand(term, command);
          command = '';
          break;
        case '\u007F': // Backspace (DEL)
          // Do not delete the prompt
          if (term._core.buffer.x > 2) {
            term.write('\b \b');
            if (command.length > 0) {
              command = command.substr(0, command.length - 1);
            }
          }
          break;
        default: // Print all other characters for demo
          if (e >= String.fromCharCode(0x20) && e <= String.fromCharCode(0x7E) || e >= '\u00a0') {
            command += e;
            term.write(e);
          }
      }
    });


link hanlding
    // Create a very simple link provider which hardcodes links for certain lines
    term.registerLinkProvider({
      provideLinks(bufferLineNumber, callback) {
        switch (bufferLineNumber) {
          case 2:
            callback([
              {
                text: 'VS Code',
                range: { start: { x: 28, y: 2 }, end: { x: 34, y: 2 } },
                activate() {
                  window.open('https://github.com/microsoft/vscode', '_blank');
                }
              },
              {
                text: 'Hyper',
                range: { start: { x: 37, y: 2 }, end: { x: 41, y: 2 } },
                activate() {
                  window.open('https://github.com/vercel/hyper', '_blank');
                }
              },
              {
                text: 'Theia',
                range: { start: { x: 47, y: 2 }, end: { x: 51, y: 2 } },
                activate() {
                  window.open('https://github.com/eclipse-theia/theia', '_blank');
                }
              }
            ]);
            return;
          case 8:
            callback([
              {
                text: 'WebGL renderer',
                range: { start: { x: 54, y: 8 }, end: { x: 67, y: 8 } },
                activate() {
                  window.open('https://npmjs.com/package/xterm-addon-webgl', '_blank');
                }
              }
            ]);
            return;
          case 14:
            callback([
              {
                text: 'Links',
                range: { start: { x: 45, y: 14 }, end: { x: 49, y: 14 } },
                activate() {
                  window.alert('You can handle links any way you want');
                }
              },
              {
                text: 'themes',
                range: { start: { x: 52, y: 14 }, end: { x: 57, y: 14 } },
                activate() {
                  isBaseTheme = !isBaseTheme;
                  term.setOption('theme', isBaseTheme ? baseTheme : otherTheme);
                  document.querySelector('.demo .inner').classList.toggle('other-theme', !isBaseTheme);
                  term.write(`\r\nActivated ${isBaseTheme ? 'xterm.js' : 'snazzy'} theme`);
                  prompt(term);
                }
              },
              {
                text: 'addons',
                range: { start: { x: 60, y: 14 }, end: { x: 65, y: 14 } },
                activate() {
                  window.open('/docs/guides/using-addons/', '_blank');
                }
              }
            ]);
            return;
          case 15: callback([
            {
              text: 'typed API',
              range: { start: { x: 45, y: 15 }, end: { x: 53, y: 15 } },
              activate() {
                window.open('https://github.com/xtermjs/xterm.js/blob/master/typings/xterm.d.ts', '_blank');
              }
            },
            {
              text: 'decorations',
              range: { start: { x: 56, y: 15 }, end: { x: 66, y: 15 } },
              activate() {
                window.open('https://github.com/xtermjs/xterm.js/blob/master/typings/xterm.d.ts#L947', '_blank');
              }
            },
          ]);
            return;
        }
        callback(undefined);
      }
    });
  }

*/

export default wrasse;