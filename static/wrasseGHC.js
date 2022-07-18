

const termMap = {
    "infinite type" : `Infinite Type
    
    A type is constructed using itself.
    This is not possible as type checking
    must end.`,
    "ambiguous occurrence" : `Ambiguous occurrence
    
    The compiler cannot tell which of
    the mentioned terms is the one that
    you want to use`,
}

const wrasseGHC = {
    regex : new RegExp(
            Object.keys(termMap)
            .map(x => `(${x})`)
            .join('|')
        ),
    map : termMap,
}

export default wrasseGHC;