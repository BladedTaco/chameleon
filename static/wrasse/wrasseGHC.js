

const termMap = {
    "infinite type" : `Infinite Type
    
    A type is constructed using itself.
    This is not possible as type checking
    must end at a concrete type.`,
    
    "Ambiguous occurrence" : `Ambiguous occurrence
    
    The compiler cannot tell which of
    the mentioned terms is the one that
    you want to use`,
}

const wrasseGHC = {
    regex : {
        keyword : new RegExp(
            Object.keys(termMap)
            .map(x => `(${x})`)
            .join('|')
        , 'gi'),
        symbol : /‘(?<symbol>[a-zA-Z.0-9]+)’/g,
        location : /generated\/Infile.hs:(?<line>[0-9]+):(?<colStart>[0-9]+)(?:\-(?<colEnd>[0-9]+))?/g,
        ambiguous : /‘(?<namespace>([a-zA-Z.0-9]+\.)+)(?<symbol>[a-zA-Z.0-9]+)’/g,
        error : /\[(?<code>GHC-[0-9]+)\]/g,
        codeCommit : /\[\[commit\]\]/g,
    },
    map : termMap,
    
}

export default wrasseGHC;