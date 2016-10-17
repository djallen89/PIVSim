namespace PIVSim

open PIVSim.Units

module Universe = 

    type Universe =
        member this.hourglass = 0L<ns>
        member this.particles: Set<Particles.Particle> = Set.empty
        
    let tick universe = 0
    
