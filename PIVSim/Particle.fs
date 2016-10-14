namespace PIVSim

open Units
open Vectors

module Particles = 

    type Particle(mass: float<amu>, velocity: Velocity, position: Position, charge: float<C>) = 
    
        member this.mass with get() = mass
        member this.velocity with get() = velocity
        member this.position with get() = position
        member this.charge with get() = charge

    (*
     * type Photon(wavelength: double<nm>, velocity: Velocity<nm/nanosecond>, position: Vector3D<nm>) =
     * member this.wavelength with get() = wavelength
     *  member this.velocity with get() = velocity
     *  member this.position with get() = position
*)
