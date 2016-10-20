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

    let elec_force_pair (p1: Particle) (p2: Particle) =
        if p1 = p2 then
            failwith "Cannot calculate electric force of a particle on itself"
        let numerator = GIGA_COULOMB_CNST * p1.charge * p2.charge //nano e 
        let r_sq =  particle_dist_mag_sq p1.position p2.position
        let magnitude = numerator / r_sq
        //head precedes tail, a ---> b 
        let components = distance_unit_vector p2.position p1.position
        Vector3D(magnitude, components)

    let component_displacement = 0

    let intersection (particle_a: Particle) (particle_b: Particle) =
        if particle_a.velocity = particle_b.velocity then
            None
        else
            let t = 0 //time at which collision occurs
            Some(t)

