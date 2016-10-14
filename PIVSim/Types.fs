namespace PIVSim

module Units =
    [<Measure>] type amu
    [<Measure>] type nm
    [<Measure>] type ns
    [<Measure>] type C

module Vectors =
    open Units
    
    type Vector3D(x: float, y: float, z: float) = 
        member this.magnitude = sqrt (x + y + z)
        member this.x with get() = x
        member this.y with get() = y
        member this.z with get() = z
    
    type Velocity(scalar: float<nm/ns>, vector: Vector3D) =
        member this.scalar with get() = scalar
        member this.vector with get() = vector

    type Position(scalar: float<nm>, vector: Vector3D) =
        member this.scalar with get() = scalar
        member this.vector with get() = vector

    type Displacement(vector: Vector3D) =
        member this.unit_vector =
            let denom = vector.magnitude
            new Vector3D(vector.x / denom, vector.y / denom, vector.z / denom)
        member this.vector with get() = vector
