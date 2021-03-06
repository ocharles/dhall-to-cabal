-- This file is auto-generated by dhall-to-cabal-meta. Look but don't touch (unless you want your edits to be over-written).
  ∀(SPDX : Type)
→ ∀ ( license
    :   ∀(id : ./SPDX/LicenseId.dhall)
      → ∀(exception : Optional ./SPDX/LicenseExceptionId.dhall)
      → SPDX
    )
→ ∀ ( licenseVersionOrLater
    :   ∀(id : ./SPDX/LicenseId.dhall)
      → ∀(exception : Optional ./SPDX/LicenseExceptionId.dhall)
      → SPDX
    )
→ ∀ ( ref
    :   ∀(ref : Text)
      → ∀(exception : Optional ./SPDX/LicenseExceptionId.dhall)
      → SPDX
    )
→ ∀ ( refWithFile
    :   ∀(ref : Text)
      → ∀(file : Text)
      → ∀(exception : Optional ./SPDX/LicenseExceptionId.dhall)
      → SPDX
    )
→ ∀(and : SPDX → SPDX → SPDX)
→ ∀(or : SPDX → SPDX → SPDX)
→ SPDX
