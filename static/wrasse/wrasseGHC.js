
// a dict of terms and their short descriptions.
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

// the exported object
const wrasseGHC = {
    // regexes for various thigns
    regex : {
        // regex for termMap keywords
        keyword : new RegExp(
            Object.keys(termMap)
            .map(x => `(${x})`)
            .join('|')
        , 'gi'),
        // regex for symbol defined in the code
        symbol : /‘(?<symbol>[a-zA-Z.0-9]+)’/g,
        // regex for code location
        location : /generated\/Infile.hs:(?<line>[0-9]+)(?:\-(?<lineEnd>[0-9]+))?:(?<colStart>[0-9]+)(?:\-(?<colEnd>[0-9]+))?/g,
        // regex for ambiguous occurrence suggested fixes
        ambiguous : /(?:^(?:either|or) )‘(?<namespace>([a-zA-Z.0-9]+\.)+)(?<symbol>[a-zA-Z.0-9]+)’/g,
        // regex for HEMI error code
        error : /\[(?<code>GHC-[0-9]+)\]/g,
        // regex for code commit suggested fix.
        codeCommit : /\[\[commit\]\]/g,
    },
    map : termMap,
    
}

export default wrasseGHC;