namespace PIVSim

module Units =
    [<Measure>] type amu
    [<Measure>] type nm
    [<Measure>] type ns
    [<Measure>] type C

module Math =
    type Axis = X | Y | Z

module Vectors =
    open Units

    let vertice_order(ax: float, ay, az, bx, by, bz) : int = 
        let factor a b : int =
            if a < b then -1
            elif a = b then 0
            else 0
        match (ax, ay, az, bx, by, bz) with
            | (x1, y1, z1, x2, y2, z2) when x1 = x2 && y1 = y2 && z1 = z2 -> 0
            | (x1, y1, z1, x2, y2, z2) -> 4 * factor x1 x2 + 2 * factor y1 y2 + factor z1 z2

    type ComponentVec(x: float, y: float, z: float) =
        member this.norm with get() = sqrt (this.x + this.y + this.z)

        member this.x with get() = x
        member this.y with get() = y
        member this.z with get() = z

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

    let sub_determinant axis (vec_a: ComponentVec) (vec_b: ComponentVec) =
        match axis with
            | Math.Axis.X -> vec_a.y * vec_b.z - vec_b.y * vec_a.z
            | Math.Axis.Y -> vec_a.z * vec_b.x - vec_b.z * vec_a.x
            | Math.Axis.Z -> vec_a.x * vec_b.y - vec_b.x * vec_a.y

    let dot_prod (vec_a: ComponentVec) (vec_b: ComponentVec) =
        vec_a.x * vec_b.x + vec_a.y * vec_b.y + vec_a.z * vec_b.z

    let cross_prod (vec_a: ComponentVec) (vec_b: ComponentVec) =
        ComponentVec(sub_determinant Math.Axis.X vec_a vec_b,
                     sub_determinant Math.Axis.Y vec_a vec_b,
                     sub_determinant Math.Axis.Z vec_a vec_b)

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

