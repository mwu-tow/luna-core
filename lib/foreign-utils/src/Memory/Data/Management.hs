{-# LANGUAGE TypeInType #-}

module Memory.Data.Management where



-------------------------------
-- === Memory management === --
-------------------------------

-- === Definition === --

type family Management (a :: k) :: ManagementType

data ManagementType
    = Managed
    | Unmanaged

type AssertUnmanaged a = (Management a ~ 'Unmanaged)
type AssertManaged   a = (Management a ~ 'Managed)
