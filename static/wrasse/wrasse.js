import { Terminal as xTerminal } from 'xterm'
import { FitAddon } from 'xterm-addon-fit';
import ESC from './ansiEscapes';
import wrasseGHC from './wrasseGHC';
import tWin from './terminalWindows' ;
import {sleep, clamp, within, start_pattern_gen, null_func, deep_copy, debounce} from './util';
import Split from 'split-grid';

const DEBUG = false;

// start uninitialized
let initialized = false;
// create the xterm terminal in the html, with addons
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

// html objects
const html = {
    terminal : document.getElementById('terminal-container'),
    buttons : {
    },
    hover : {
      shell : document.getElementById("wrasse-hover"),
      content : document.getElementById("wrasse-hover-content"),
    },
    block: document.getElementById("wrasse-block"),
}

// scrolls terminal to the top
let scrollToTop = () => {
  wrasse.window.line = 0;
  wrasse.window.scroll = 0;
  wrasse.window.requestDraw();
}

// fits terminal to the outer div
const fitTerminal = () => {
  // get proposed dimensions, and fit if they dont match current dimensions
  const dims = fitAddon.proposeDimensions()
  if (wrasse.terminal.rows != dims.rows || wrasse.terminal.cols != dims.cols) {
    fitAddon.fit();
  }
  // expand initial window, then layered windows
  wrasse.window.expand(true);
  perm.windows?.[0]?.resize(wrasse.window.width / 2 - 1, wrasse.window.height, false, true)
  wrasse.window.resize(wrasse.window.width / 2, wrasse.window.height, false, true)
  perm.windows?.[0]?.move(wrasse.window.width + 2, 0, false, true)
}

// sets up the wrasse environment
let wrasse_setup = () => {
    // html element for resizing
    Split({
        minSize: 100,
        snapOffset: 10,
        rowGutters: [{
            track: 1,
            element: document.querySelector('.gutter-row-1'),
        }]
    })
    
    if (DEBUG) console.log('wrasse init');

    // open terminal and make main window
    wrasse.terminal.open(html.terminal);
    wrasse.window = new tWin.Window(WrasseTerminal, 2, 2, 10, 10);
    initialized = true;

    // ignore all keypresses
    // wrasse.terminal.attachCustomKeyEventHandler((_) => false);
    wrasse.terminal.modes.mouseTrackingMode = "any"
    wrasse.terminal.modes.wraparoundMode = true;

    // Make the terminal's size and geometry fit the size of #terminal-container
    fitTerminal();
    // ... dynamically
    new ResizeObserver( debounce(() => fitTerminal(), 100)).observe(html.terminal)

    // write starter text
    wrasse.window.write(`${ESC.colouredText(ESC.Colour.Yellow, ESC.Colour.LtDkGrey, "Wrasse")} is loading, please wait.\ntype-check the code to start if nothing happens after a few seconds.\n`, scrollToTop)

    // setup mouse followers
    const onMouseMove = (e) =>{
      // have hover shell follow mouse
      html.hover.shell.style.left = e.pageX + 'px';
      html.hover.shell.style.top = e.pageY + 'px';

      // offset if hidden
      let off = 0;
      if (html.block.classList.contains("hidden")) {
        off = 100;
      }

      // have block stay under mouse if not hidden
      html.block.style.left = (e.pageX - off) + 'px';
      html.block.style.top = (e.pageY - off) + 'px';
    }

    // listen to mouse move
    document.addEventListener('mousemove', onMouseMove);

    // load messages
    (async () => {
      // poll backend server for messages
      let response = await fetch('/messages', {
        method: 'POST',
        body: 'messages',
      });

      if (DEBUG) console.log(data);
      
      // await data in json format
      let data = response.json();
      wrasse.messages = await data;
    })();
}

// main hook/entrypoint for wrasse in the chameleonIDE
let hook = async ({code, response, editor}) => {
    // await code
    let data = await response;

    // setup wrasse
    wrasse.editor = editor;
    if (DEBUG) console.log('back-end call');
    if (!initialized) {
        wrasse.setup()
    } 

    // send code to ghc handler
    let ghc_data = await handle_ghc(code);

    if (DEBUG) console.log(ghc_data);

    // reformat received data
    wrasse.tree = parse_tree(ghc_data.full)
    wrasse.tree.symbols = parse_symbols(wrasse.tree)
    wrasse.data_0 = ghc_data
    wrasse.data_0.ghc.code = code.split("\n")
    wrasse.data_1 = data
    wrasse.data_2 = { ghc: ghc_data, chameleon: data }

    if (DEBUG) console.log(wrasse.tree);

    // start with interactive terminal window
    interactive_terminal(wrasse.tree)

    // delete all extraneous xterm layers
    html.terminal.querySelectorAll('.xterm-selection-layer, .xterm-link-layer, .xterm-cursor-layer').forEach(x => x.remove())
}

// finds a subtree in the wrasse tree 
// call as find_child(tree, "subtree", "subsubtree", ...)
const find_child = (tree, ...content) => {
  // base cases and sanity checks
  if (tree == undefined || tree == {}) return {};
  if (content.length == 0) return tree;
  
  // recursively find subtree by descending one level a call
  return find_child(tree.children.find(x => x.content === content[0]), ...content.slice(1))
}

// parses the symbols in the tree under DeferGHC/symbols into JSON data
let parse_symbols = (tree) => {
  // get symbols in tree
  const symbols = find_child(tree, "Defer GHC", "symbols")?.children ?? [];
  // parse into json data
  return symbols.map(x => JSON.parse(x.content))
}

// parse the backend data into the wrasse tree
let parse_tree = (tree) => {
  // parse a single node of the tree
  const parse_node = (node) => {
    return {
      content: node[0][0],
      active: node[0][1],
      line: node[0][2],
      // recurse with children
      children: node[1].map(parse_node),
      link: undefined,
    }
  }

  // get the parsed tree, and make Root/GHC/Output all expanded on open
  const parsedTree = parse_node(tree);
  const parsedTree_GHC = find_child(parsedTree, "GHC");
  const parsedTree_GHC_out = find_child(parsedTree_GHC, "output");

  parsedTree.active = true;
  parsedTree_GHC.active = true;
  parsedTree_GHC_out.active = true;

  return parsedTree;
}

// switch active terminal (deprecated?)
let switch_terminal = (data) => {
  // clean disposables
    perm.disposables.forEach(x => x.dispose())
    perm.disposables = [];
    perm.keywords.forEach(x => x.dispose())
    perm.keywords = [];
    perm.windows = [];

    // reset terminal & windows
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

    // star drawing for fun
    let starGen = (function *() {
      while (true) {
        for (const item of ["|", "/", "-", "\\"]) {
          yield item;
       }
      }
    })();

    // natural number generator
    let numGen = (function *() {
      let i = 0
      while (true) {
        yield ++i;
      }
    })();
    
    // callback recursive star drawing function
    let recurse = async () => {
      sleep(100);
      wrasse.window.write(ESC.cursorSavePosition 
        + ESC.cursorTo((numGen.next().value % 2) + 30, 1) + starGen.next().value
        + ESC.cursorRestorePosition
        , recurse)
    }
    // dont call it, this was a debug thing at one point
    // recurse();
}

// sets the content of the right wrasse window
const set_hover_content = (text) => {
  // only populate if text is valid and nonempty
  if (typeof text !== 'undefined' && text !== "") { 
    // update text in window
    perm.windows[0].reset()
      .writeln(text || "");
  }
}

// blocks/unblocks the mouse from clicking and interacting (deprecated)
const block_mouse = (bool) => {
  if (!bool) {
    // hide element
    html.block.classList.add("hidden")
  } else { 
    // show element
    html.block.classList.remove("hidden")
  }
}

// the tree view terminal
let interactive_terminal = (tree) => {
  // clear terminal
  // wrasse.terminal.reset()
  switch_terminal();

  // create the right window
  let hoverWin = new tWin.Window(
    wrasse.terminal,
    wrasse.terminal.cols / 2,
    0,
    wrasse.terminal.cols / 2 - 2,
    wrasse.terminal.rows,
    {movable : true, softwrap: true}
  );
  perm.windows.push(hoverWin);
  fitTerminal();

  // line tracking for link registering functions
  let curr_line = 0;

  // sets the head of each subtree in the recursive terminal
  const setHead = (x, head) => {
    if (x.content !== "Root" || x === head) {
      x.head = head;
      x.children.forEach(el => setHead(el, head));
    }
  }

  // writes the text into the terminal
  const write_text = (node, level, write = false) => {
    let line = curr_line
    
    // handle padding
    let prefix = "  ".repeat(level) 
    let node_string = node.content //JSON.stringify(node)// node.content
    // wrasse.window.writeln(ESC.colouredText({r:100, g:200, b:100}, {}, node_string));

    // write tree string
    if (write) {
      if (node.children.length == 0) {
        // list element
        wrasse.window.writeln(prefix + "- " + node_string);
      } else {
        if (node.active) {
          // expanded element
          wrasse.window.writeln(prefix + "▼ " + node_string);
        } else {
          // expandable element
          wrasse.window.writeln(prefix + "► " + node_string);
        }
      }
    }

    // give and increment line
    node.line = line;
    curr_line++;

    // recurse if active
    if (node.active) {
      node.children.forEach(x => write_text(x, level+1, write))
    }
  }

  // cleans lines from terminal on retraction of node
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

  // registers tree control highlights in the terminal
  const register_links = (node, level, ignoreLine) => {
    // register link provider
    node.link = {
      text: node.content,
      // start and end position of line content in terminal
      range : { 
        start: { x: level*2+ 1,                        y: node.line },
        end:   { x: level*2 + 2 + node.content.length, y: node.line } 
      },
    }

    // if not a leaf or to be ignored
    if (node.children.length > 0 && node.line != ignoreLine) {
      let hovered = false;
      // add the highlight
      wrasse.window.addLink(
        node.link.range,
        {
          click(link) {
            // remove all link providers except this one
            link.window.links
              // .filter(x => x !== link)
              .forEach(x => {
                x.highlight.fg.seq = ""
                x.highlight.bg.seq = ""
                x.highlight.resetFG.seq = ""
                x.highlight.resetBG.seq = ""
              });
            link.window.content.forEach(line => line.esc = line.esc.filter(esc => esc.seq !== ""))
            // link.window.links = [link]
            link.window.links = []

            // fix terminal output
            clean_lines(node, level)

            // flip state
            node.active = !node.active;

            // update tree state
            curr_line = 0
            write_text(tree, 0, false)
            // register_links(tree, 0, node.line);
            register_links(tree, 0);

            // perm.keywords.forEach(x => x.dispose())
            // perm.keywords = [];
            register_keywords(tree)

            // write line
            wrasse.window.write(ESC.cursorTo(0, node.line));
          },
          enter(link) {
            // set right window to show expandable/collapsible nature
            wrasse.set_hover_content(node.active
              ? "click to collapse"
              : "click to expand")
          },
          leave(link) {
            // release hover content if needed
            hovered = false;
            wrasse.set_hover_content()
          }
        },
        ESC.Colour.Blue
      ).treelink = true; // mark it as a link for expanding/collapsing the tree
    }

    // recurse if node is active
    if (node.active) {
      node.children.forEach(x => register_links(x, level+1))
    }
  }
  
  // registers other types of highlights in the terminal
  const register_keywords = (node) => {
    // if a valid node
    if (node?.link) {
      const {text, range} = node.link;
      // ambiguous
      for (const match of text.matchAll(wrasseGHC.regex.ambiguous)) {
        // add a link for each highlight
        const {namespace, symbol} = match.groups;
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 1,                y: node.line },
            end:   { x: range.start.x + match.index + match[0].length,  y: node.line } 
          },
          {
            click(link) {
              // on click, attach subtree
              if (node.active) {
                node.children = node.children.filter(x => x.content !== "Root");
                node.active = false;
                wrasse.interactive_terminal(wrasse.tree);
                return;
              }

              // star drawing for loading
              let starGen = start_pattern_gen();
              let finished = false;
              let recurse = () => {
                sleep(10).then(() => {
                  // exit when data received
                  if (finished) return;

                  wrasse.window.write(ESC.cursorSavePosition 
                    + ESC.cursorTo(range.start.x - 1, node.line) + starGen.next().value
                    + ESC.cursorRestorePosition
                    , recurse)
                })
              }
              recurse();

              // request subtree backend call
              (async () => {
                if (DEBUG) console.log(wrasse);

                // get code
                let code = wrasse.data_0?.ghc.code
                // apply ambiguous occurrence patches
                .map((x) => {
                  let arr = x.split('=');
                  if (arr.length <= 1) {
                    return x;
                  }
                  let new_x = arr.slice(1).join("=");
                  return arr[0] + "=" + new_x.replace(symbol, `${namespace}${symbol}`);
                })
                // reduce into single string
                .reduce((acc, curr) => {
                  return acc + '\n' + curr;
                })

                if (DEBUG) console.log(code);

                // make backend call
                let response = await fetch('/ghc', {
                  method: 'POST',
                  body: code,
                });
                let data = await response.json();

                if (DEBUG) console.log(data);
                // parse subtree
                const add = parse_tree(data.full);
                add.symbols = parse_symbols(add);

                // set subtree head
                setHead(add, add);
                
                // allow for code committing
                const cd = find_child(add, "GHC", "code");
                cd.content = "code [[commit]]"
                cd.code = code
                node.children.push(add);
                node.active = true;

                // end loading animation
                finished = true;

                // retain position through terminal update
                const [l, s] = [wrasse.window.line, wrasse.window.scroll];

                // update terminal
                wrasse.interactive_terminal(wrasse.tree);

                // restore position as above
                wrasse.window.line = l;
                wrasse.window.scroll = s;
              })();
            },
            enter(link) {
              // tooltip text
              wrasse.set_hover_content(`See what happens if you use '${namespace + symbol}'`)
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
        // add a link for each keyword
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 2,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line } 
          },
          {
            click(link) {
              // not clickable
            },
            enter(link) {
              // set tooltip to term description
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
        // add a link for each highlight
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 2,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line } 
          },
          {
            click(link) {
              // commit code to main window
              wrasse.editor(node?.code)
            },
            enter(link) {
              // tooltip
              wrasse.set_hover_content("Commit code to main window")
            },
            leave(link) {
              wrasse.set_hover_content()
            }
          },
          ESC.Colour.Green
        );
      }

      // symbols
      for (const match of text.matchAll(wrasseGHC.regex.symbol)) {
        // check if symbol is known
        const {symbol} = match.groups;
        const sym = node.head.symbols.find(x => x.symbolName === symbol);
        if (sym) {
          // add a link for each symbol highlight
          wrasse.window.addLink(
            { 
              start: { x: range.start.x + match.index + 2,                y: node.line },
              end:   { x: range.start.x + match.index + match[0].length,  y: node.line } 
            },
            {
              click(link) {
                // no click action
              },
              enter(link) {
                let codeline = ""

                // if the symbol is a user symbol, get its code location and the text of that line
                const match2 = sym?.symbolDefinedAt?.[1]?.matchAll(wrasseGHC.regex.location)?.next()?.value
                if (match2?.groups) {
                  const {line, colStart, colEnd} = match2.groups;
                  
                  if (wrasse?.data_0?.ghc?.code) {
                    codeline = wrasse?.data_0?.ghc?.code[line-1];
                  }
                }

                // update tooltip based on extracted information
                wrasse.set_hover_content(`${symbol}\n` +
                  `    type: ${sym?.symbolType || "constant"}\n` +
                  `    signature: ${sym?.definition || symbol}\n` +
                  `    defined ${sym?.symbolDefinedAt?.[0] === "in" ? "in package" : "at"}: ${sym?.symbolDefinedAt?.[1] || "unknown"}\n` +
                  (codeline ? `     ╚═► ${codeline}\n` : "") +
                  `    etc.:` + `\n${sym?.symbolEtc}\n`.split("\n").join("\n      ")
                )
              },
              leave(link) {
                wrasse.set_hover_content()
              }
            },
            ESC.Colour.LightGrey
          );
        }
      }

      // code locations
      for (const match of text.matchAll(wrasseGHC.regex.location)) {
        // add a link for each code location highlight
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 1,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line }
          },
          {
            click(link) {
              // no click action
            },
            enter(link) {
              // extract code positions
              const {line, colStart, colEnd} = match.groups;
              
              // attempt to get line of code
              let x = ""
              if (wrasse?.data_0?.ghc?.code) {
                x = wrasse?.data_0?.ghc?.code[line-1];
              }

              // set tooltip
              wrasse.set_hover_content(`line ${line}, column ${colStart} to ${colEnd}\n`+
              `   ${x}`)
            },
            leave(link) {
              wrasse.set_hover_content()
            }
          } 
        );
      }

      // Error code
      for (const match of text.matchAll(wrasseGHC.regex.error)) {
        // add a link for each error code
        wrasse.window.addLink(
          { 
            start: { x: range.start.x + match.index + 1,                    y: node.line },
            end:   { x: range.start.x + match.index + match[0].length + 1,  y: node.line }
          },
          {
            click(link) {
              // no click action
            },
            enter(link) {
              // get error code and find in error code database
              const {code} = match.groups;
              let msg = wrasse.messages.find((x) => x.errCode == code);

              if (msg) {
                // extract information
                const {summary, removed, bodyText, introduced, errCode, severity, extension, flag, title, examples} = msg
                
                // nests one string after another like a tree
                const nest = (x) => x.split("\n").join("\n|   ")

                // get examples as before code and after code to a string
                const ex = examples.map(({beforeCode, explanation, errorMsg, exTitle, afterCode}) => `${ESC.colouredText(ESC.Colour.Yellow, {}, exTitle)}\n`+
                nest(`before:\n${beforeCode}`) 
                + `\n\n`+
                nest(`after:\n${afterCode}`))

                // get output string, filtering out missing information lines
                const flags = [severity, extension, flag, introduced, removed]
                const subtitle = [
                  // match flags to colour
                  `severity: ${ESC.colouredText(
                    {error : ESC.Colour.Red, warning : ESC.Colour.Yellow}[severity] ?? ESC.Colour.Green, {}, 
                    severity)}`,
                  `originates from: ${extension}`,
                  `requires compiler flag: ${flag}`,
                  `introduced in ${introduced}`,
                  `removed in ${removed}`,
                ].filter((x, i) => flags[i] != "")
                .join("; ")

                // set tooltip
                wrasse.set_hover_content(`[${errCode}] -> ${title}\n`+
                `${subtitle}\n\n`+
                `${summary}\n\n`+
                `${bodyText}\n\n`+
                `examples:${["", ...ex].join("\n" + "-".repeat(10) + "\n")}\n`
                )

                // wrasse.set_hover_content(JSON.stringify(msg, null, 2))
              } else {
                // set tooltip to error string
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
    // recursve if active
    if (node.active) {
      node?.children.forEach(register_keywords)
    }
  }
  // set tree head
  setHead(tree, tree)
  // write text to terminal
  write_text(tree, 0, true);
  // register tree control highlights
  register_links(tree, 0)
  // register other highlights
  register_keywords(tree)
}

// passthrough wrapper (old use was deprecated)
let handle_ghc = async (code) => {
    return ghc_hook(code)
}

// basic async call to wrasse backend
let ghc_hook = async (code) => {
    let response = await fetch('/ghc', {
        method: 'POST',
        body: code,
    });
    return response.json();
}

// ethereal elements that last past their closure
const perm = {
  "disposables": [],
  "keywords": [],
  "windows": [],
}

// export list
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

export default wrasse;