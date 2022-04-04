import React from 'react';
import ReactDOM from 'react-dom';
import {
  initializeEditor,
  highlight,
  drawAnnotations,
  clearDecorations,
} from './editor';
import { Provider, useSelector, useDispatch } from 'react-redux';
import store from './store';
import { arrEq } from './helper';
import {
  BASIC_MODE,
  typeCheckThunk,
  switchTaskThunk,
  switchTaskNextRoundThunk,
  disableHighlight,
  prevStep,
  nextStep,
  setStep,
} from './store';
import tasks from './code';
import Modal from 'react-modal';
import Split from 'split-grid'

// --------------------------------------------------------------
// --------------------------------------------------------------

import wrasse from './wrasse'

// --------------------------------------------------------------
// --------------------------------------------------------------

Split({
  columnGutters: [{
    track: 1,
    element: document.querySelector('#gutter'),
  }],
})

Modal.setAppElement('#debugger');

let currentTask = 0;
let currentRound = 1;
let editor = initializeEditor(tasks[currentTask]);
store.dispatch(switchTaskThunk(currentTask));

editor.on('focus', function () {
  store.dispatch(disableHighlight());
});

store.subscribe(() => {
  let {
    currentStep,
    mode,
    showHighlights,
    prevLocs,
    nextLocs,
    round
  } = store.getState();
  let nohighligt = { from: { line: 0, ch: 0 }, to: { line: 0, ch: 0 } };

  clearDecorations(editor);
  if (round !== currentRound) {
    currentRound = round;
    document.getElementById('skip').classList.toggle('hidden')
    document.getElementById('giveup').classList.toggle('hidden')

  }
  if (!showHighlights) return;
  if (currentStep === null) return;

  if (mode === BASIC_MODE) {
    highlight(
      nohighligt,
      nohighligt,
      [currentStep.locA, ...prevLocs],
      [currentStep.locB, ...nextLocs],
      editor,
    );
  } else {
    highlight(currentStep.locA, currentStep.locB, prevLocs, nextLocs, editor);
    drawAnnotations(
      currentStep.locA,
      currentStep.locB,
      currentStep.text,
      editor,
    );
  }
});

document.getElementById('save').addEventListener('click', _ => {
  let text = editor.getValue();
  store.dispatch(typeCheckThunk(text));
  // --------------------------------------------------------------
  // --------------------------------------------------------------
  // wrasse.hook("he");
  wrasse.hook(store.dispatch(typeCheckThunk(text)));
  
  // --------------------------------------------------------------
  // --------------------------------------------------------------
});

document.getElementById('skip').addEventListener('click', _ => {
  let state = store.getState()
  if (state.round === 2) return
  if (state.currentTaskNum < 7) {
    let nextTask = state.currentTaskNum + 1
    store.dispatch(switchTaskThunk(nextTask))
    editor.setValue(tasks.at(nextTask))
  } else if (state.currentTaskNum === 7) {
    let nextTask = state.pending.at(0)
    store.dispatch(switchTaskNextRoundThunk(nextTask))
    editor.setValue(tasks.at(nextTask))

  }
});

document.getElementById('giveup').addEventListener('click', _ => {
  let state = store.getState()
  let currentPendingIndex = state.pending.findIndex(v => v === state.currentTaskNum)
  if (state.round === 1) return
  if (state.currentTaskNum < 7) {
    let nextTask = state.pending.at(currentPendingIndex + 1)
    store.dispatch(switchTaskThunk(nextTask))
    editor.setValue(tasks.at(nextTask))
  } else if (state.currentTaskNum === 7) {
    window.location =
      'https://docs.google.com/forms/d/e/1FAIpQLSfmXyASOPW2HIK-Oqp5nELBTltKeqZjqQ0G9JFram8eUCx26A/viewform?usp=sf_link';
  }
});

document.querySelectorAll('.example').forEach(elem => {
  elem.addEventListener('click', e => {
    let taskId = parseInt(e.target.dataset['taskId'], 10);
    editor.setValue(tasks.at(taskId));
    store.dispatch(switchTaskThunk(taskId));
  });
});

const TypeSig = ({ type }) => {
  if (type.tag === "TypeForm"
    && type.contents.length === 3
    && type.contents.at(0).contents === '['
    && type.contents.at(1).contents === 'Char'
    && type.contents.at(2).contents === ']'
  ) {
    return <span className="inline-block">String</span>
  } else if (type.tag === "TypeForm") {
    return <span className="inline-block">{type.contents.map(t => <TypeSig type={t}></TypeSig>)}</span>
  } else if (type.tag === "TypeFormPart" && type.contents === ' ') {
    return <span className="inline-block w-1"></span>
  }

  else if (type.tag === "TypeFormPart") {
    return <span className="inline-block">{type.contents}</span>
  }
}

const ModelContent = () => {
  let dispatch = useDispatch();
  let currentTaskNum = useSelector(state => state.currentTaskNum);
  let pending = useSelector(state => state.pending)
  let round = useSelector(state => state.round)

  return (
    <div className='flex flex-col justify-around items-center h-full'>
      <div>
        <p className='text-center'>
          Congratulations. You fixed the type error!
        </p>
        {pending.length === 0 ? (
          <p className='text-center'>Click next to leave us some feedback.</p>
        ) : (
          <p className='text-center'>
            Click next to head over to the next challenge.
          </p>
        )}
      </div>
      <button
        className='px-5 py-1 bg-green-400 rounded-md'
        onClick={() => {
          if (pending.length === 0) {
            window.location =
                'https://docs.google.com/forms/d/e/1FAIpQLSfmXyASOPW2HIK-Oqp5nELBTltKeqZjqQ0G9JFram8eUCx26A/viewform?usp=sf_link';
            return;
          }
          if (round === 1 && currentTaskNum < 7) {
            let nextTask = currentTaskNum + 1
            dispatch(switchTaskThunk(nextTask));
            editor.setValue(tasks.at(nextTask));

          } else if (round === 1 && currentTaskNum === 7) {
            let nextTask = pending.at(0)
            dispatch(switchTaskNextRoundThunk(nextTask));
            editor.setValue(tasks.at(nextTask));
          } else if (round === 2) {
            let nextPendingIndex = pending.findIndex(n => n > currentTaskNum)
            if (nextPendingIndex === -1) {
              window.location =
                'https://docs.google.com/forms/d/e/1FAIpQLSfmXyASOPW2HIK-Oqp5nELBTltKeqZjqQ0G9JFram8eUCx26A/viewform?usp=sf_link';
            } else {
              let nextTask = pending.at(nextPendingIndex)
              dispatch(switchTaskThunk(nextTask));
              editor.setValue(tasks.at(nextTask));
            }
          }
        }}
      >
        Next
      </button>
    </div>
  );
};

const Debuger = () => {
  let mode = useSelector(state => state.mode);
  let wellTyped = useSelector(state => state.wellTyped);
  let loadError = useSelector(state => state.loadError);
  let parseError = useSelector(state => state.parseError);
  return (
    <>
      <Modal
        isOpen={wellTyped}
        className='max-w-2xl bg-gray-100 h-80 min-w-max left-1/2 top-1/2 -translate-y-1/2 -translate-x-1/2 absolute p-6 rounded-md'
      >
        <ModelContent />
      </Modal>
      {(() => {
        if (parseError !== null) {
          return <ParseErrorReport />;
        } else if (loadError !== null) {
          return <LoadErrorReport />;
        } else if (!wellTyped) {
          return <TypeErrorReport />;
        }
      })()}
    </>
  );
};

const ParseErrorReport = () => {
  let parseError = useSelector(state => state.parseError);
  return <div class="p-4">
    <p className='py-2 px-4'>A syntax error was found in the code</p>
    <div className='bg-gray-100 py-2 px-4 rounded-md'>
      <p> {parseError.message} </p>
      <p>Location: {parseError.loc.srcLine}:{parseError.loc.srcColumn}</p>
    </div>
  </div>;
};

const LoadErrorReport = () => {
  let loadError = useSelector(state => state.loadError);
  return <div class="p-4">
    <p className='py-2 px-4'>A variable is used without being declared.</p>
    {loadError.map(m => {
      return <div className='bg-gray-100 py-2 px-4 rounded-md'>
        <p>Variable: {m.at(0)} </p>
        <p>Location:{" "}
          {" " + m.at(1).srcSpanStartLine}:{m.at(1).srcSpanStartColumn} -
          {" " + m.at(1).srcSpanEndLine}:{m.at(1).srcSpanEndColumn}
        </p>

      </div>
    })}
  </div >;
}

const TypeErrorReport = () => {
  let mode = useSelector(state => state.mode);
  return (
    <div className='p-2 flex flex-col items-start' style={{ fontFamily: 'IBM Plex Sans' }}>
      <div className='mb-2 bg-gray-100 px-4 py-2 rounded-md w-auto'>
        Current mode:
        {mode === BASIC_MODE ? ' Basic Mode' : ' Interactive Mode'}
      </div>
      <Message></Message>
      {mode === BASIC_MODE ? null : <TypingTable></TypingTable>}
    </div>
  );
};

const Message = () => {
  let contextItem = useSelector(state => state.currentContextItem);
  return contextItem === null ? null : (
    <div className='mb-5'>
      <div className='text-md italic my-2'>
        Chameleon cannot infer a type for the expression below:
      </div>

      <div className='my-1 text-sm'>
        Expression:
        <span className='code ml-2 px-1 rounded-md bg-gray-700 text-white inline-block'>
          {contextItem['contextExp']}
        </span>
      </div>
      <div className='my-1 text-sm'>
        <span className='w-14 inline-block'>Type 1: </span>
        <span className='code groupMarkerB rounded-sm px-0.5 cursor-pointer'>
          {/* {unAlias(contextItem['contextType1'])} */}
          <TypeSig type={contextItem.contextType1}></TypeSig>
        </span>
      </div>
      <div className='my-1 text-sm'>
        <span className='w-14 inline-block'>Type 2: </span>
        <span className='code groupMarkerA rounded-sm px-0.5 cursor-pointer'>
          {/* {unAlias(contextItem['contextType2'])} */}
          <TypeSig type={contextItem.contextType2}></TypeSig>

        </span>
      </div>
    </div>
  );
};

const TypingTable = () => {
  let dispatch = useDispatch();
  let context = useSelector(state => state.context);
  return (
    <div
      className={'grid gap-1 context-grid text-xs w-full'}
      style={{ fontFamily: 'JetBrains Mono' }}
    >
      <div className='text-center'>
        <ion-icon
          onClick={() => dispatch(prevStep())}
          style={{ fontSize: 20, cursor: 'pointer' }}
          name='arrow-up-circle'
        ></ion-icon>
      </div>
      <div className='text-center'>TYPE 1</div>
      <div className='text-center'>EXPRESSION</div>
      <div className='text-center'>TYPE 2</div>
      {context.map((row, i) => (
        <ContextRow row={row} key={i}></ContextRow>
      ))}
      <div className='text-center'>
        <ion-icon
          onClick={() => dispatch(nextStep())}
          style={{ fontSize: 20, cursor: 'pointer' }}
          name='arrow-down-circle'
        ></ion-icon>
      </div>
      <div className='text-center'></div>
      <div className='text-center'></div>
      <div className='text-center'></div>
    </div>
  );
};

const ContextRow = ({ row }) => {
  let currentTraverseId = useSelector(state => state.currentTraverseId);
  let steps = useSelector(state => state.steps);
  let dispatch = useDispatch();
  let { contextExp, contextType1, contextType2, contextSteps } = row;
  let affinity = contextSteps
    .find(step => arrEq(step.at(0), currentTraverseId))
    .at(1);
  let affinityClass =
    affinity === 'R' ? 'sideA' : affinity === 'L' ? 'sideB' : 'sideAB';
  let firstReleventStepTId = contextSteps.find(i => i.at(2)).at(0);

  let lastReleventStepTId = contextSteps
    .slice()
    .reverse()
    .find(i => i.at(2))
    .at(0);

  let firstReleventStep = steps.findIndex(step =>
    arrEq(step['stepId'], firstReleventStepTId),
  );
  let lastReleventStep = steps.findIndex(step =>
    arrEq(step['stepId'], lastReleventStepTId),
  );
  return (
    <>
      <Stepper rowInfo={contextSteps}></Stepper>
      <div
        onClick={() => dispatch(setStep(firstReleventStep))}
        className='rounded-sm p-1 groupMarkerB flex justify-center items-center cursor-pointer'
      >
        <TypeSig type={contextType1}></TypeSig>
      </div>
      <div
        className={
          'rounded-sm p-1 flex justify-center items-center ' + affinityClass
        }
      >
        {contextExp}
      </div>
      <div
        onClick={() => dispatch(setStep(lastReleventStep))}
        className='rounded-sm p-1 groupMarkerA flex justify-center items-center cursor-pointer'
      >
        <TypeSig type={contextType2}></TypeSig>
      </div>
    </>
  );
};

const Stepper = ({ rowInfo }) => {
  let steps = useSelector(state => state.steps);
  let currentTraverseId = useSelector(state => state.currentTraverseId);
  let stepsInRow = rowInfo.filter(ri => ri[2]);
  let dispatch = useDispatch();
  return (
    <>
      <div className='flex flex-col justify-center items-center'>
        {stepsInRow.map(([traverseId, _z1, _z2]) => {
          let stepId = steps.findIndex(step =>
            arrEq(step['stepId'], traverseId),
          );
          return (
            <div
              key={stepId}
              onClick={() => dispatch(setStep(stepId))}
              className={
                'rounded-lg w-4 h-4 my-0.5 p-0.5 cursor-pointer text-xs leading-3 text-center ' +
                (arrEq(traverseId, currentTraverseId)
                  ? 'bg-green-400'
                  : 'bg-gray-400')
              }
            >
              {stepId + 1}
            </div>
          );
        })}
      </div>
    </>
  );
};

ReactDOM.render(
  <Provider store={store}>
    <Debuger></Debuger>
  </Provider>,
  document.getElementById('debugger'),
);
