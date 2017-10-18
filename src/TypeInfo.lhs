%
%
%

Specifying type / marshalling information

\begin{code}
module TypeInfo 

       ( TypeInfo(..)
       , typeInfos

       , v_bool_ti
       , variant_ti
       , mb_currency_ti
       , mb_date_ti
       , guid_ti
       , iid_ti
       , clsid_ti
       , bstr_ti

       ) where

import BasicTypes
import NativeInfo
import Opts
import Data.Maybe
import AbsHUtils
import AbstractH ( Type )
import LibUtils  ( comLib )
#ifdef SUPPORT_TYPELIBS
import
       Automation ( VARENUM(..) )
#endif

\end{code}

A @TypeInfo@ record contains all the info needed by the
backend(s) to convert the use of a type into appropriate
Haskell code. 

\begin{code}
data TypeInfo 
 = TypeInfo {
     type_name        :: String,
     haskell_type     :: QualName,
     marshaller       :: QualName,
     copy_marshaller  :: QualName,
     unmarshaller     :: QualName,
     ref_marshaller   :: QualName,
     ref_unmarshaller :: QualName,
     alloc_type       :: Maybe QualName,
     free_type        :: Maybe QualName,
     prim_type        :: Type,
     c_type           :: String,
     prim_size        :: QualName,
     prim_sizeof      :: Int,
     prim_align       :: Int,
     auto_type        :: QualName,
#ifdef SUPPORT_TYPELIBS
     auto_vt          :: Maybe VARENUM,
#endif
     is_pointed       :: Bool,
     finalised        :: Bool,
     attributes       :: Maybe String
   }
   deriving ( Show, Eq )

\end{code}

\begin{code}
typeInfos :: [TypeInfo]
typeInfos = 
  [ variant_ti
  , v_bool_ti
  , currency_ti
  , iid_ti
  , clsid_ti
  , guid_ti
  ]

iid_ti :: TypeInfo
iid_ti =
    TypeInfo 
        { type_name        = "IID"
        , haskell_type     = toQualName "System.Win32.Com.IID a"
        , marshaller       = toQualName "System.Win32.Com.marshallIID"
        , copy_marshaller  = toQualName "System.Win32.Com.copyIID"
        , unmarshaller     = toQualName "System.Win32.Com.unmarshallIID"
        , ref_marshaller   = toQualName "System.Win32.Com.writeIID"
        , ref_unmarshaller = toQualName "System.Win32.Com.readIID"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyForeignPtr (tyQCon comLib "IID" [uniqueTyVar "a"])
        , c_type           = "IID*"
        , auto_type        = toQualName "System.Win32.Com.IID a"
        , prim_size        = toQualName "System.Win32.Com.sizeofIID"
        , prim_sizeof      = 16
        , prim_align       = 4
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Nothing
#endif
        , is_pointed       = True
        , finalised        = True
        , attributes       = Nothing
        }

clsid_ti :: TypeInfo
clsid_ti = 
  TypeInfo 
        { type_name        = "CLSID"
        , haskell_type     = toQualName "System.Win32.Com.CLSID"
        , marshaller       = toQualName "System.Win32.Com.marshallCLSID"
        , copy_marshaller  = toQualName "System.Win32.Com.copyCLSID"
        , unmarshaller     = toQualName "System.Win32.Com.unmarshallCLSID"
        , ref_marshaller   = toQualName "System.Win32.Com.writeCLSID"
        , ref_unmarshaller = toQualName "System.Win32.Com.readCLSID"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyForeignPtr (tyQConst comLib "CLSID")
        , c_type           = "CLSID*"
        , auto_type        = toQualName "System.Win32.Com.CLSID"
        , prim_size        = toQualName "System.Win32.Com.sizeofCLSID"
        , prim_sizeof      = 16
        , prim_align       = 4
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Nothing
#endif
        , is_pointed       = True
        , finalised        = True
        , attributes       = Nothing
        }

guid_ti :: TypeInfo
guid_ti = 
  TypeInfo 
        { type_name        = "GUID"
        , haskell_type     = toQualName "System.Win32.Com.GUID"
        , marshaller       = toQualName "System.Win32.Com.marshallGUID"
        , copy_marshaller  = toQualName "System.Win32.Com.copyGUID"
        , unmarshaller     = toQualName "System.Win32.Com.unmarshallGUID"
        , ref_marshaller   = toQualName "System.Win32.Com.writeGUID"
        , ref_unmarshaller = toQualName "System.Win32.Com.readGUID"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyForeignPtr (tyQConst comLib "GUID")
        , c_type           = "GUID*"
        , auto_type        = toQualName "System.Win32.Com.GUID"
        , prim_size        = toQualName "System.Win32.Com.sizeofGUID"
        , prim_sizeof      = 16
        , prim_align       = 4
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Nothing
#endif
        , is_pointed       = True
        , finalised        = True
        , attributes       = Nothing
        }

mb_currency_ti :: Maybe TypeInfo
mb_currency_ti = Just currency_ti

currency_ti :: TypeInfo
currency_ti = 
  TypeInfo 
        { type_name        = "CURRENCY"
        , haskell_type     = toQualName "System.Win32.Com.Automation.Currency"
        , marshaller       = toQualName "System.Win32.Com.Automation.marshallCurrency"
        , copy_marshaller  = toQualName "System.Win32.Com.Automation.marshallCurrency"
        , unmarshaller     = toQualName "System.Win32.Com.Automation.unmarshallCurrency"
        , ref_marshaller   = toQualName "System.Win32.Com.Automation.writeCurrency"
        , ref_unmarshaller = toQualName "System.Win32.Com.Automation.readCurrency"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyInt64
        , c_type           = "CURRENCY"
        , auto_type        = toQualName "System.Win32.Com.Automation.Currency"
        , prim_size        = toQualName "System.Win32.Com.HDirect.HDirect.sizeofInt64"
        , prim_sizeof      = lONGLONG_SIZE
        , prim_align       = lONGLONG_ALIGN_MODULUS
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Just VT_CY
#endif
        , is_pointed       = False
        , finalised        = False
        , attributes       = Nothing
        }

mb_date_ti :: Maybe TypeInfo
mb_date_ti = Just date_ti

date_ti :: TypeInfo
date_ti = 
  TypeInfo 
        { type_name        = "DATE"
        , haskell_type     = toQualName "System.Win32.Com.Automation.Date"
        , marshaller       = toQualName "System.Win32.Com.HDirect.HDirect.marshallDouble"
        , copy_marshaller  = toQualName "System.Win32.Com.HDirect.HDirect.marshallDouble"
        , unmarshaller     = toQualName "System.Win32.Com.HDirect.HDirect.unmarshallDouble"
        , ref_marshaller   = toQualName "System.Win32.Com.HDirect.HDirect.writeDouble"
        , ref_unmarshaller = toQualName "System.Win32.Com.HDirect.HDirect.readDouble"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyDouble
        , c_type           = "double"
        , auto_type        = toQualName "System.Win32.Com.Automation.Date"
        , prim_size        = toQualName "System.Win32.Com.HDirect.HDirect.sizeofDouble"
        , prim_sizeof      = dOUBLE_SIZE
        , prim_align       = dOUBLE_ALIGN_MODULUS
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Just VT_DATE
#endif
        , is_pointed       = False
        , finalised        = False
        , attributes       = Nothing
        }

variant_ti :: TypeInfo
variant_ti 
  | optNoOverloadVariant || optServer =
    TypeInfo 
        { type_name        = "VARIANT"
        , haskell_type     = toQualName "System.Win32.Com.Automation.VARIANT"
        , marshaller       = toQualName "System.Win32.Com.Automation.marshallVARIANT"
        , copy_marshaller  = toQualName "System.Win32.Com.Automation.copyVARIANT"
        , unmarshaller     = toQualName "System.Win32.Com.Automation.unmarshallVARIANT"
        , ref_marshaller   = toQualName "System.Win32.Com.Automation.writeVARIANT"
        , ref_unmarshaller = toQualName "System.Win32.Com.Automation.readVARIANT"
        , alloc_type       = Just (toQualName "System.Win32.Com.Automation.allocVARIANT")
        , free_type        = Nothing
        , prim_type        = {-tyPtr-} (mkTyConst $ toQualName "System.Win32.Com.Automation.VARIANT")
        , c_type           = "VARIANT"
        , auto_type        = toQualName "a"
        , prim_size        = toQualName "System.Win32.Com.Automation.sizeofVARIANT"
        , prim_sizeof      = 16
        , prim_align       = 8
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Just VT_VARIANT
#endif
        , is_pointed       = True
        , finalised        = False
        , attributes       = Nothing
        }
  | otherwise =
    TypeInfo 
        { type_name        = "VARIANT"
        , haskell_type     = toQualName "a"  -- magic.
        , marshaller       = toQualName "System.Win32.Com.Automation.marshallVariant"
        , copy_marshaller  = toQualName "System.Win32.Com.Automation.marshallVariant"
        , unmarshaller     = toQualName "System.Win32.Com.Automation.unmarshallVariant"
            -- Note: when we're marshalling Variants by reference, this
            --       is only done for constructed types, so we want to use
            --       the non-overloaded VARIANT marshallers rather than
            --       the overloaded (since VARIANTs embedded inside a
            --       constructed type is represented by VARIANT.)
        , ref_marshaller   = toQualName "System.Win32.Com.Automation.writeVARIANT"
        , ref_unmarshaller = toQualName "System.Win32.Com.Automation.readVARIANT"
        , alloc_type       = Just (toQualName "System.Win32.Com.Automation.allocVARIANT")
        , free_type        = Nothing
        , prim_type        = {-tyPtr-} (mkTyConst $ toQualName "System.Win32.Com.Automation.VARIANT")
        , c_type           = "VARIANT"
        , auto_type        = toQualName "a"
        , prim_size        = toQualName "System.Win32.Com.Automation.sizeofVARIANT"
        , prim_sizeof      = 16
        , prim_align       = 8
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Just VT_VARIANT
#endif
        , is_pointed       = True
        , finalised        = False
        , attributes       = Nothing
        }

v_bool_ti :: TypeInfo
v_bool_ti = 
  TypeInfo 
        { type_name        = "VARIANT_BOOL"
        , haskell_type     = toQualName "Prelude.Bool"
        , marshaller       = toQualName "System.Win32.Com.Automation.marshallVARIANT_BOOL"
        , copy_marshaller  = toQualName "System.Win32.Com.Automation.marshallVARIANT_BOOL"
        , unmarshaller     = toQualName "System.Win32.Com.Automation.unmarshallVARIANT_BOOL"
        , ref_marshaller   = toQualName "System.Win32.Com.Automation.writeVARIANT_BOOL"
        , ref_unmarshaller = toQualName "System.Win32.Com.Automation.readVARIANT_BOOL"
        , alloc_type       = Nothing
        , free_type        = Nothing
        , prim_type        = tyInt16
        , c_type           = "VARIANT_BOOL"
        , auto_type        = toQualName "Prelude.Bool"
        , prim_size        = toQualName "System.Win32.Com.HDirect.HDirect.sizeofInt16"
        , prim_sizeof      = sHORT_SIZE
        , prim_align       = sHORT_ALIGN_MODULUS
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Nothing
#endif
        , is_pointed       = False
        , finalised        = False
        , attributes       = Nothing
        }

bstr_ti :: TypeInfo
bstr_ti = 
  TypeInfo 
        { type_name        = "BSTR"
        , haskell_type     = toQualName "Prelude.String"
        , marshaller       = toQualName "System.Win32.Com.marshallBSTR"
        , copy_marshaller  = toQualName "System.Win32.Com.marshallBSTR"
        , unmarshaller     = toQualName "System.Win32.Com.unmarshallBSTR"
        , ref_marshaller   = toQualName "System.Win32.Com.writeBSTR"
        , ref_unmarshaller = toQualName "System.Win32.Com.readBSTR"
        , alloc_type       = Nothing
        , free_type        = Just (toQualName "System.Win32.Com.freeBSTR")
        , prim_type        = tyPtr tyString
        , c_type           = "void*"
        , auto_type        = toQualName "Prelude.String"
        , prim_size        = toQualName "System.Win32.Com.HDirect.HDirect.sizeofPtr"
        , prim_sizeof      = dATA_PTR_SIZE
        , prim_align       = dATA_PTR_ALIGN_MODULUS
#ifdef SUPPORT_TYPELIBS
        , auto_vt          = Just VT_BSTR
#endif
        , is_pointed       = False
        , finalised        = False
        , attributes       = Nothing
        }

\end{code}
