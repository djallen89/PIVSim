namespace PIVSim

open MathNet.Numerics.LinearAlgebra.Double

module Units =
    [<Measure>] type amu
    [<Measure>] type nm
    [<Measure>] type ns
    [<Measure>] type C

module Math =
    type Axis = X | Y | Z

module Vectors =
    open Units

    let vertice_order(ax, ay, az, bx, by, bz) : int = 
        let factor a b : int =
            if a < b then -1
            elif a = b then 0
            else 1
        match (ax, ay, az, bx, by, bz) with
            | (x1, y1, z1, x2, y2, z2) when x1 = x2 && y1 = y2 && z1 = z2 -> 0
            | (x1, y1, z1, x2, y2, z2) -> 4 * factor x1 x2 + 2 * factor y1 y2 + factor z1 z2

    type ComponentVec(coords: float[]) = 
        member this.vec = DenseVector.OfArray coords
        member this.x = this.vec.[0]
        member this.y = this.vec.[1]
        member this.z = this.vec.[2]

        override lhs.Equals(rhs) =
            match rhs with
                | :? ComponentVec as rhs -> lhs.x = rhs.x && lhs.y = rhs.y && lhs.z = rhs.z
                | _ -> false

        override lhs.GetHashCode() = hash [lhs.x, lhs.y, lhs.z]

        interface System.IComparable with
            member lhs.CompareTo rhs =
                match rhs with
                    | :? ComponentVec as rhs -> vertice_order(lhs.x, lhs.y, lhs.z, rhs.x, rhs.y, rhs.z) 
                    | _ -> invalidArg "rhs" "cannot compare values of different types"

    let diff_sq head tail = (head - tail) * (head - tail)

    let particle_dist_mag_sq (head: ComponentVec) (tail: ComponentVec) =
        diff_sq head.x tail.x + diff_sq head.y tail.y + diff_sq head.z tail.z

    let particle_dist_mag (head: ComponentVec) (tail: ComponentVec) =
        sqrt(particle_dist_mag_sq head tail)

    let distance_unit_vector (head: ComponentVec) (tail: ComponentVec) =
        let denom = particle_dist_mag head tail
        let new_vec = [| tail.vec.[0] / denom ; tail.vec.[1] / denom ; tail.vec.[2] / denom |]
        ComponentVec(new_vec) 

    let sub_determinant axis (vec_a: ComponentVec) (vec_b: ComponentVec): float =
        match axis with
            | Math.Axis.X -> vec_a.y * vec_b.z - vec_b.y * vec_a.z
            | Math.Axis.Y -> vec_a.z * vec_b.x - vec_b.z * vec_a.x
            | Math.Axis.Z -> vec_a.x * vec_b.y - vec_b.x * vec_a.y

    type Vector3D(scalar: float, components: ComponentVec) =
        member this.scalar with get() = scalar
        member this.components with get() = components

        override lhs.Equals(rhs) =
            match rhs with
                | :? Vector3D as rhs -> lhs.scalar = rhs.scalar && lhs.components = rhs.components
                | _ -> false

        override lhs.GetHashCode() = hash lhs.components

        interface System.IComparable with
            member lhs.CompareTo(rhs) =
                match rhs with
                    | :? Vector3D as rhs ->
                        let component_comparison = compare lhs.components rhs.components
                        let scalar_comparison = compare lhs.scalar rhs.scalar
                        match (component_comparison, scalar_comparison) with
                            | (c_cmp, s_cmp) when c_cmp = 0 && s_cmp = 0 -> 0
                            | (c_cmp, s_cmp) when c_cmp = 0 -> s_cmp
                            | (c_cmp, _) -> c_cmp
                    | _ -> invalidArg "rhs" "cannot compare values of different types"

