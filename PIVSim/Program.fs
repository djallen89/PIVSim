namespace PIVSim

open System
open Units

module Main = 

   [<EntryPoint>]
   let main argv =
       let m = 12.7<amu>
       
       let speed = 7.2
       let v_x = 1.3
       let v_y = -9.0
       let v_z = 3.0
       let velo = new Vectors.Vector3D(speed, new Vectors.ComponentVec(v_x, v_y, v_z))
       
       let x = 1.0
       let y = 0.0
       let z = 3.0
       let position = new Vectors.ComponentVec(x, y, z)
                                     
       let charge = 1.0<C>
       
       let first_particle = Particles.Particle(m, velo, position, charge)
       printfn "%f" first_particle.velocity.scalar
//       var light = new Particles.Photon(501<nm/nanosecond>
       0
