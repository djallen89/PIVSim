namespace PIVSim

module Constants = 
    [<Measure>]
    type m
    [<Measure>]
    type sec

    [<Literal>]
    let celeritas: float<m/sec> = 3.0e8<m/sec>
