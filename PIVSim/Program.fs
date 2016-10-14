namespace PIVSim

open System
open Units

module Main = 

   [<EntryPoint>]
   let main argv =
       let m = 12.7<amu>
       
       let speed = 7.2<nm/ns>
       let v_x = 1.3
       let v_y = -9.0
       let v_z = 3.0
       let velo = new Vectors.Velocity(speed, new Vectors.Vector3D(v_x, v_y, v_z))
       
       let x = 0.3333
       let y = 0.0
       let z = 1.0
       let pos_scalar = 3.0<nm>
       let position = new Vectors.Position(pos_scalar, new Vectors.Vector3D(x, y, z))
                                     
       let charge = 1.0<C>
       
       let first_particle = new Particles.Particle(m, velo, position, charge)
       printfn "The speed is %f" (first_particle.velocity.scalar / 1.0<nm/ns>)
//       var light = new Particles.Photon(501<nm/nanosecond>
       0
