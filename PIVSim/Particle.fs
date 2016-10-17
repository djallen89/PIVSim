namespace PIVSim

open Units
open Vectors
open Math

module Particles = 
    type Particle(mass: float<amu>, velocity: Vector3D, position: ComponentVec, charge: float<C>) = 
        member this.mass with get() = mass
        member this.velocity with get() = velocity
        member this.position with get() = position
        member this.charge with get() = charge

        member this.component_displacement axis =
            this.velocity.scalar
            * match axis with
                | Math.Axis.X -> this.velocity.components.x
                | Math.Axis.Y -> this.velocity.components.y
                | Math.Axis.Z -> this.velocity.components.z                

        override lhs.Equals(rhs) =
            match rhs with
                | :? Particle as rhs -> lhs.mass = rhs.mass 
                                     && lhs.velocity = rhs.velocity
                                     && lhs.position = rhs.position
                                     && lhs.charge = rhs.charge
                | _ -> false

        override lhs.GetHashCode() = hash lhs.position

        interface System.IComparable with
            member lhs.CompareTo(rhs) = 
                match rhs with
                    | :? Particle as rhs -> 
                        match ((lhs.mass / 1.0<amu>).CompareTo(rhs.mass / 1.0<amu>),
                               compare lhs.velocity rhs.velocity,
                               compare lhs.position rhs.position,
                               (lhs.charge / 1.0<C>).CompareTo(rhs.charge / 1.0<C>)) with
                            | (0, 0, 0, 0) -> 0
                            | (cmp_m, _, _, _) when not (cmp_m = 0) -> cmp_m
                            | (_, cmp_v, _, _) when not (cmp_v = 0) -> cmp_v
                            | (_, _, cmp_p, _) when not (cmp_p = 0) -> cmp_p
                            | (_, _, _, cmp_q) -> cmp_q

                    | _ -> invalidArg "rhs" "cannot compare values of different types"

    let displacement (ith_particle: Particle) =
        ComponentVec(ith_particle.component_displacement(Math.Axis.X),
                     ith_particle.component_displacement(Math.Axis.Y),
                     ith_particle.component_displacement(Math.Axis.Z))

    let intersection (particle_a: Particle) (particle_b: Particle) =
        if particle_a.velocity = particle_b.velocity then
            None
        else
            let displacement_a = displacement particle_a
            let displacement_b = displacement particle_b
            //solve particle_a.components displacement_a particle_b.components displacement_b
            let t = 0 //time at which collision occurs
            Some(t)

