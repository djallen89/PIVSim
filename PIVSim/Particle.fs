namespace PIVSim

open Constants
open Units
open Vectors
open Math

module Particles =
    type SubAtomic =
        | NEUTRON
        | PROTON

    let subatomic_mass p =
        match p with
            | SubAtomic.NEUTRON -> mass_neutron
            | SubAtomic.PROTON -> mass_proton

    type Nucleus(z: byte, n: byte) =
        member this.z with get() = z
        member this.n with get() = n
        member this.mass = float this.n * mass_neutron + float this.z * mass_proton

    type Atom(nucleus: Nucleus, electrons: uint16) =
        member this.nucleus with get() = nucleus
        member this.electrons with get() = electrons
        member this.mass = this.nucleus.mass

        member this.radius =
            match this.nucleus.z with
                | 1uy ->  Some(RADIUS_HYDROGEN)
                | 2uy ->  Some(RADIUS_HELIUM)
                | 3uy ->  Some(RADIUS_LITHIUM)
                | 6uy ->  Some(RADIUS_CARBON)
                | 7uy ->  Some(RADIUS_NITROGEN)
                | 8uy ->  Some(RADIUS_OXYGEN)
                | 9uy ->  Some(RADIUS_FLUORINE)
                | 10uy -> Some(RADIUS_ARGON)
                | 15uy -> Some(RADIUS_PHOSPHORUS)
                | 16uy -> Some(RADIUS_SULFUR)
                | 17uy -> Some(RADIUS_CHLORINE)
                | 18uy -> Some(RADIUS_ARGON)
                | 35uy -> Some(RADIUS_BROMINE)
                | 36uy -> Some(RADIUS_KRYPTON)
                | 53uy -> Some(RADIUS_IODINE)
                | 54uy -> Some(RADIUS_XENON)
                | _ -> None

    type Molecule =
        | MONATOMIC of Atom 
        | DIATOMIC of Atom

    let molecular_mass p =
        match p with
            | Molecule.MONATOMIC m -> m.mass
            | Molecule.DIATOMIC d -> 2.0 * d.mass

    let molecular_radius p =
        match p with 
            | Molecule.MONATOMIC a | Molecule.DIATOMIC a -> a.radius

    type ParticleKind =
        | BETA
        | S of SubAtomic 
        | NU of Nucleus 
        | M of Molecule

    let particle_mass p =
        match p with
            | ParticleKind.BETA -> None
            | ParticleKind.S t -> Some(subatomic_mass t)
            | ParticleKind.NU n -> Some(n.mass)
            | ParticleKind.M m -> Some(molecular_mass m)

    let particle_radius p =
        match p with
            | ParticleKind.BETA -> None
            | ParticleKind.S _ -> None
            | ParticleKind.NU n -> Some(radius_baryon)
            | ParticleKind.M m -> molecular_radius m
        
    type Particle(particle_kind: ParticleKind,
                  velocity: Vector3D,
                  position: ComponentVec,
                  charge: float) =
        member this.particle_kind with get() = particle_kind
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
                | :? Particle as rhs -> lhs.particle_kind = rhs.particle_kind
                                     && lhs.velocity = rhs.velocity
                                     && lhs.position = rhs.position
                | _ -> false

        override lhs.GetHashCode() = hash lhs.position

        interface System.IComparable with
            member lhs.CompareTo(rhs) = 
                match rhs with
                    | :? Particle as rhs -> compare lhs.position rhs.position
                    | _ -> invalidArg "rhs" "cannot compare values of different types"

    let displacement (ith_particle: Particle) =
        ComponentVec [|ith_particle.component_displacement(Math.Axis.X);
                      ith_particle.component_displacement(Math.Axis.Y);
                      ith_particle.component_displacement(Math.Axis.Z)|]

    let elec_force_pair (p1: Particle) (p2: Particle) =
        GIGA_COULOMB_CNST * p1.charge * p2.charge /
                          particle_dist_sq p1.position p2.position

    let intersection (particle_a: Particle) (particle_b: Particle) =
        if particle_a.velocity = particle_b.velocity then
            None
        else
            let displacement_a = displacement particle_a
            let displacement_b = displacement particle_b
            //solve particle_a.components displacement_a particle_b.components displacement_b
            let t = 0 //time at which collision occurs
            Some(t)

