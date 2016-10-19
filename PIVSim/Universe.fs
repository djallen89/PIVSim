namespace PIVSim

open PIVSim.Constants
open PIVSim.Particles
open PIVSim.Units

module Universe = 

    type Snapshot =
        { particles : Set<Particles.Particle> }

    let elec_force_on p1 (snapshot: Snapshot) =
        if snapshot.particles.Contains p1 then
            failwith "Cannot calculate force on particle from itself!"
        Set.fold (fun force_sum ith_p -> force_sum + elec_force_pair p1 ith_p) 0.0 snapshot.particles

    type Universe(moments: Snapshot array) =
        member this.moments = moments
        member this.hourglass = moments.Length
        
    let tick current_snapshot = 0
//        let next_snapshot = Set.map 
    
