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

        member this.x = coords.[0]
        member this.y = coords.[1]
        member this.z = coords.[2]

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

    let particle_dist_sq (lhs: ComponentVec) (rhs: ComponentVec) =
        let diff_sq a b = (a - b) * (a - b)
        diff_sq lhs.x rhs.x + diff_sq lhs.y rhs.y + diff_sq lhs.z rhs.z

    let particle_dist (lhs: ComponentVec) (rhs: ComponentVec) =
        sqrt(particle_dist_sq lhs rhs)
        
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

