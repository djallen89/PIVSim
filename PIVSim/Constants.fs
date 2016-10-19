namespace PIVSim

module Constants = 
    [<Measure>]
    type m
    [<Measure>]
    type sec
    [<Measure>]
    type amu

    [<Literal>]
    let celeritas: float = 3.0e8 //m/s
    [<Literal>]
    let mass_proton: float =  1.007276466879 //amu
    [<Literal>]
    let mass_neutron: float = 1.008664915885 //amu
//    [<Literal>]
//    let mass_beta: float = 5.48579909070e-4 //amu
    [<Literal>]
    let radius_baryon: float = 0.875 //fm

    [<Literal>]
    let RADIUS_HYDROGEN  : float = 120000.0 //Z: float =  1
    [<Literal>]                   
    let RADIUS_HELIUM    : float = 140000.0 //Z: float =  2
    [<Literal>]                   
    let RADIUS_FLUORINE  : float = 147000.0 //Z: float =  9
    [<Literal>]                   
    let RADIUS_OXYGEN    : float = 152000.0 //Z: float =  8
    [<Literal>]                   
    let RADIUS_NEON      : float = 154000.0 //Z: float =  10
    [<Literal>]                   
    let RADIUS_NITROGEN  : float = 155000.0 //Z: float =  7
    [<Literal>]                   
    let RADIUS_CARBON    : float = 170000.0 //Z: float =  6
    [<Literal>]                   
    let RADIUS_CHLORINE  : float = 175000.0 //Z: float =  17
    [<Literal>]                   
    let RADIUS_PHOSPHORUS: float = 180000.0 //Z: float =  15
    [<Literal>]                   
    let RADIUS_SULFUR    : float = 180000.0 //Z: float =  16
    [<Literal>]                   
    let RADIUS_LITHIUM   : float = 182000.0 //Z: float =  3
    [<Literal>]                   
    let RADIUS_ARGON     : float = 188000.0 //Z: float =  18
    [<Literal>]                   
    let RADIUS_BROMINE   : float = 185000.0 //Z: float =  35
    [<Literal>]                   
    let RADIUS_IODINE    : float = 198000.0 //Z: float =  53
    [<Literal>]                   
    let RADIUS_KRYPTON   : float = 202000.0 //Z: float =  36
    [<Literal>]                   
    let RADIUS_XENON     : float = 216000.0 //z = 54

    [<Literal>]
    let COLOUMB_CONSTANT : float = 8.9875517873681764e+9 // m/F
    [<Literal>]
    let GIGA_COULOMB_CNST: float = 8.9875517873681764 // Gm/F
    [<Literal>]
    let VAC_PERMITTIVITY : float = 8.854187817e-12 // F/m
    [<Literal>]
    let ELEMENTARY_CHARGE: float = 1.602176208e-19 // C
    [<Literal>]
    let NANO_E_CHARGE    : float = 1.602176208e-10 // nC
