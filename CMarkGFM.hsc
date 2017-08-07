{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

module CMarkGFM (
    commonmarkToHtml
  , commonmarkToXml
  , commonmarkToMan
  , commonmarkToLaTeX
  , commonmarkToNode
  , nodeToHtml
  , nodeToXml
  , nodeToMan
  , nodeToLaTeX
  , nodeToCommonmark
  , optSourcePos
  , optHardBreaks
  , optSmart
  , optSafe
  , extStrikethrough
  , extTable
  , extAutolink
  , extTagfilter
  , Node(..)
  , NodeType(..)
  , PosInfo(..)
  , DelimType(..)
  , ListType(..)
  , ListAttributes(..)
  , Url
  , Title
  , Level
  , Info
  , TableCellAlignment(..)
  , CMarkOption
  , CMarkExtension
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString, withCString)
import qualified System.IO.Unsafe as Unsafe
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text, empty)
import qualified Data.Text.Foreign as TF
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative ((<$>), (<*>))

#include <cmark.h>
#include <core-extensions.h>

-- | Ensure core extensions are registered.
ensurePluginsRegistered :: IO ()
ensurePluginsRegistered = c_core_extensions_ensure_registered

-- | Frees a cmark linked list, produced by extsToLlist.
freeLlist :: LlistPtr a -> IO ()
freeLlist = c_cmark_llist_free c_CMARK_DEFAULT_MEM_ALLOCATOR

-- | Converts a list of resolved extension pointers to a single cmark
-- linked list, which can be passed to functions requiring a list of
-- extensions.
extsToLlist :: [ExtensionPtr] -> IO (LlistPtr ExtensionPtr)
extsToLlist [] = return nullPtr
extsToLlist (h:t) = do
  t' <- extsToLlist t
  c_cmark_llist_append c_CMARK_DEFAULT_MEM_ALLOCATOR t' (castPtr h)

-- | Resolves CMarkExtensions to pointers.
resolveExts :: [CMarkExtension] -> IO [ExtensionPtr]
resolveExts exts = do
  ensurePluginsRegistered
  mapM resolveExt exts
  where resolveExt ext = do p <- withCString (unCMarkExtension ext) c_cmark_find_syntax_extension
                            if p == nullPtr then
                              fail $ "could not load extension " ++ unCMarkExtension ext
                            else
                              return p

-- | Convert CommonMark formatted text to Html, using cmark's
-- built-in renderer.
commonmarkToHtml :: [CMarkOption] -> [CMarkExtension] -> Text -> Text
commonmarkToHtml opts exts =
  commonmarkToX render_html opts exts Nothing
  where exts' = Unsafe.unsafePerformIO $ resolveExts exts
        render_html n o _ = do
          llist <- extsToLlist exts'
          r <- c_cmark_render_html n o llist
          freeLlist llist
          return r

-- | Convert CommonMark formatted text to CommonMark XML, using cmark's
-- built-in renderer.
commonmarkToXml :: [CMarkOption] -> [CMarkExtension] -> Text -> Text
commonmarkToXml opts exts = commonmarkToX render_xml opts exts Nothing
  where render_xml n o _ = c_cmark_render_xml n o

-- | Convert CommonMark formatted text to groff man, using cmark's
-- built-in renderer.
commonmarkToMan :: [CMarkOption] -> [CMarkExtension] -> Maybe Int -> Text -> Text
commonmarkToMan = commonmarkToX c_cmark_render_man

-- | Convert CommonMark formatted text to latex, using cmark's
-- built-in renderer.
commonmarkToLaTeX :: [CMarkOption] -> [CMarkExtension] -> Maybe Int -> Text -> Text
commonmarkToLaTeX = commonmarkToX c_cmark_render_latex

-- | Convert CommonMark formatted text to a structured 'Node' tree,
-- which can be transformed or rendered using Haskell code.
commonmarkToNode :: [CMarkOption] -> [CMarkExtension] -> Text -> Node
commonmarkToNode opts exts s = Unsafe.unsafePerformIO $ do
  exts' <- resolveExts exts
  parser <- c_cmark_parser_new (combineOptions opts)
  mapM_ (c_cmark_parser_attach_syntax_extension parser) exts'
  TF.withCStringLen s $! \(ptr, len) ->
             c_cmark_parser_feed parser ptr len
  nptr <- c_cmark_parser_finish parser
  c_cmark_parser_free parser
  fptr <- newForeignPtr c_cmark_node_free nptr
  withForeignPtr fptr toNode

nodeToHtml :: [CMarkOption] -> [CMarkExtension] -> Node -> Text
nodeToHtml opts exts =
  nodeToX render_html opts Nothing
  where exts' = Unsafe.unsafePerformIO $ resolveExts exts
        render_html n o _ = do
          llist <- extsToLlist exts'
          r <- c_cmark_render_html n o llist
          freeLlist llist
          return r

nodeToXml :: [CMarkOption] -> Node -> Text
nodeToXml opts = nodeToX render_xml opts Nothing
  where render_xml n o _ = c_cmark_render_xml n o

nodeToMan :: [CMarkOption] -> Maybe Int -> Node -> Text
nodeToMan = nodeToX c_cmark_render_man

nodeToLaTeX :: [CMarkOption] -> Maybe Int -> Node -> Text
nodeToLaTeX = nodeToX c_cmark_render_latex

nodeToCommonmark :: [CMarkOption] -> Maybe Int -> Node -> Text
nodeToCommonmark = nodeToX c_cmark_render_commonmark

type Renderer = NodePtr -> CInt -> Int -> IO CString

nodeToX :: Renderer -> [CMarkOption] -> Maybe Int -> Node -> Text
nodeToX renderer opts mbWidth node = Unsafe.unsafePerformIO $ do
  nptr <- fromNode node
  fptr <- newForeignPtr c_cmark_node_free nptr
  withForeignPtr fptr $ \ptr -> do
    cstr <- renderer ptr (combineOptions opts) (fromMaybe 0 mbWidth)
    TF.peekCStringLen (cstr, c_strlen cstr)

commonmarkToX :: Renderer
              -> [CMarkOption]
              -> [CMarkExtension]
              -> Maybe Int
              -> Text
              -> Text
commonmarkToX renderer opts exts mbWidth s = Unsafe.unsafePerformIO $
  TF.withCStringLen s $ \(ptr, len) -> do
    let opts' = combineOptions opts
    exts' <- resolveExts exts
    parser <- c_cmark_parser_new opts'
    mapM_ (c_cmark_parser_attach_syntax_extension parser) exts'
    c_cmark_parser_feed parser ptr len
    nptr <- c_cmark_parser_finish parser
    c_cmark_parser_free parser
    fptr <- newForeignPtr c_cmark_node_free nptr
    withForeignPtr fptr $ \p -> do
      str <- renderer p opts' (fromMaybe 0 mbWidth)
      t <- TF.peekCStringLen $! (str, c_strlen str)
      return t

data ParserPhantom
type ParserPtr = Ptr ParserPhantom

data NodePhantom
type NodePtr = Ptr NodePhantom

data LlistPhantom a
type LlistPtr a = Ptr (LlistPhantom a)

data MemPhantom
type MemPtr = Ptr MemPhantom

data ExtensionPhantom
type ExtensionPtr = Ptr ExtensionPhantom

data Node = Node (Maybe PosInfo) NodeType [Node]
     deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data DelimType =
    PERIOD_DELIM
  | PAREN_DELIM
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data ListType =
    BULLET_LIST
  | ORDERED_LIST
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data ListAttributes = ListAttributes{
    listType     :: ListType
  , listTight    :: Bool
  , listStart    :: Int
  , listDelim    :: DelimType
  } deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

type Url = Text

type Title = Text

type Level = Int

type Info = Text

type OnEnter = Text

type OnExit = Text

data TableCellAlignment = NoAlignment | LeftAligned | CenterAligned | RightAligned
     deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data NodeType =
    DOCUMENT
  | THEMATIC_BREAK
  | PARAGRAPH
  | BLOCK_QUOTE
  | HTML_BLOCK Text
  | CUSTOM_BLOCK OnEnter OnExit
  | CODE_BLOCK Info Text
  | HEADING Level
  | LIST ListAttributes
  | ITEM
  | TEXT Text
  | SOFTBREAK
  | LINEBREAK
  | HTML_INLINE Text
  | CUSTOM_INLINE OnEnter OnExit
  | CODE Text
  | EMPH
  | STRONG
  | LINK Url Title
  | IMAGE Url Title
  | STRIKETHROUGH
  | TABLE [TableCellAlignment]
  | TABLE_ROW
  | TABLE_CELL
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data PosInfo = PosInfo{ startLine   :: Int
                      , startColumn :: Int
                      , endLine     :: Int
                      , endColumn   :: Int
                      }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

newtype CMarkOption = CMarkOption { unCMarkOption :: CInt }

-- | Combine a list of options into a single option, using bitwise or.
combineOptions :: [CMarkOption] -> CInt
combineOptions = foldr ((.|.) . unCMarkOption) 0

-- | Include a @data-sourcepos@ attribute on block elements.
optSourcePos :: CMarkOption
optSourcePos = CMarkOption #const CMARK_OPT_SOURCEPOS

-- | Render @softbreak@ elements as hard line breaks.
optHardBreaks :: CMarkOption
optHardBreaks = CMarkOption #const CMARK_OPT_HARDBREAKS

-- | Convert straight quotes to curly, @---@ to em-dash, @--@ to en-dash.
optSmart :: CMarkOption
optSmart = CMarkOption #const CMARK_OPT_SMART

-- | Suppress rendering of raw HTML and potentially dangerous URLs in links
-- and images.
optSafe :: CMarkOption
optSafe = CMarkOption #const CMARK_OPT_SAFE

newtype CMarkExtension = CMarkExtension { unCMarkExtension :: String }

extStrikethrough :: CMarkExtension
extStrikethrough = CMarkExtension "strikethrough"

extTable :: CMarkExtension
extTable = CMarkExtension "table"

extAutolink :: CMarkExtension
extAutolink = CMarkExtension "autolink"

extTagfilter :: CMarkExtension
extTagfilter = CMarkExtension "tagfilter"

ptrToNodeType :: NodePtr -> IO NodeType
ptrToNodeType ptr = do
  nodeType <- c_cmark_node_get_type ptr
  case nodeType of
       #const CMARK_NODE_DOCUMENT
         -> return DOCUMENT
       #const CMARK_NODE_THEMATIC_BREAK
         -> return THEMATIC_BREAK
       #const CMARK_NODE_PARAGRAPH
         -> return PARAGRAPH
       #const CMARK_NODE_BLOCK_QUOTE
         -> return BLOCK_QUOTE
       #const CMARK_NODE_HTML_BLOCK
         -> HTML_BLOCK <$> literal
       #const CMARK_NODE_CUSTOM_BLOCK
         -> CUSTOM_BLOCK <$> onEnter <*> onExit
       #const CMARK_NODE_CODE_BLOCK
         -> CODE_BLOCK <$> info
                       <*> literal
       #const CMARK_NODE_LIST
         -> LIST <$> listAttr
       #const CMARK_NODE_ITEM
         -> return ITEM
       #const CMARK_NODE_HEADING
         -> HEADING <$> level
       #const CMARK_NODE_EMPH
         -> return EMPH
       #const CMARK_NODE_STRONG
         -> return STRONG
       #const CMARK_NODE_LINK
         -> LINK <$> url <*> title
       #const CMARK_NODE_IMAGE
         -> IMAGE <$> url <*> title
       #const CMARK_NODE_TEXT
         -> TEXT <$> literal
       #const CMARK_NODE_CODE
         -> CODE <$> literal
       #const CMARK_NODE_HTML_INLINE
         -> HTML_INLINE <$> literal
       #const CMARK_NODE_CUSTOM_INLINE
         -> CUSTOM_INLINE <$> onEnter <*> onExit
       #const CMARK_NODE_SOFTBREAK
         -> return SOFTBREAK
       #const CMARK_NODE_LINEBREAK
         -> return LINEBREAK
       _ -> if nodeType == fromIntegral (Unsafe.unsafePerformIO $ peek c_CMARK_NODE_STRIKETHROUGH) then
              return STRIKETHROUGH
            else if nodeType == fromIntegral (Unsafe.unsafePerformIO $ peek c_CMARK_NODE_TABLE) then
              TABLE <$> alignments
            else if nodeType == fromIntegral (Unsafe.unsafePerformIO $ peek c_CMARK_NODE_TABLE_ROW) then
              return TABLE_ROW
            else if nodeType == fromIntegral (Unsafe.unsafePerformIO $ peek c_CMARK_NODE_TABLE_CELL) then
              return TABLE_CELL
            else
              error $ "Unknown node type " ++ (show nodeType)
  where literal   = c_cmark_node_get_literal ptr >>= totext
        level     = c_cmark_node_get_heading_level ptr
        onEnter    = c_cmark_node_get_on_enter ptr >>= totext
        onExit     = c_cmark_node_get_on_exit  ptr >>= totext
        listAttr  = do
          listtype <- c_cmark_node_get_list_type ptr
          listdelim <- c_cmark_node_get_list_delim ptr
          tight <- c_cmark_node_get_list_tight ptr
          start <- c_cmark_node_get_list_start ptr
          return ListAttributes{
            listType  = case listtype of
                             (#const CMARK_ORDERED_LIST) -> ORDERED_LIST
                             (#const CMARK_BULLET_LIST)  -> BULLET_LIST
                             _                           -> BULLET_LIST
          , listDelim  = case listdelim of
                             (#const CMARK_PERIOD_DELIM) -> PERIOD_DELIM
                             (#const CMARK_PAREN_DELIM)  -> PAREN_DELIM
                             _                           -> PERIOD_DELIM
          , listTight  = tight
          , listStart  = start
          }
        url       = c_cmark_node_get_url ptr >>= totext
        title     = c_cmark_node_get_title ptr >>= totext
        info      = c_cmark_node_get_fence_info ptr >>= totext
        alignments = do
          ncols <- c_cmarkextensions_get_table_columns ptr
          cols <- c_cmarkextensions_get_table_alignments ptr
          mapM (fmap ucharToAlignment . peekElemOff cols) [0..(fromIntegral ncols) - 1]
        ucharToAlignment (CUChar 108) = LeftAligned
        ucharToAlignment (CUChar 99)  = CenterAligned
        ucharToAlignment (CUChar 114) = RightAligned
        ucharToAlignment _            = NoAlignment

getPosInfo :: NodePtr -> IO (Maybe PosInfo)
getPosInfo ptr = do
  startline <- c_cmark_node_get_start_line ptr
  endline <- c_cmark_node_get_end_line ptr
  startcol <- c_cmark_node_get_start_column ptr
  endcol <- c_cmark_node_get_end_column ptr
  if startline + endline + startcol + endcol == 0
     then return Nothing
     else return $ Just PosInfo{ startLine = startline
                               , startColumn = startcol
                               , endLine = endline
                               , endColumn = endcol }

toNode :: NodePtr -> IO Node
toNode ptr = do
  let handleNodes ptr' =
        if ptr' == nullPtr
           then return []
           else do
              x  <- toNode ptr'
              xs <- c_cmark_node_next ptr' >>= handleNodes
              return $! (x:xs)
  nodeType <- ptrToNodeType ptr
  children <- c_cmark_node_first_child ptr >>= handleNodes
  posinfo <- getPosInfo ptr
  return $! Node posinfo nodeType children

fromNode :: Node -> IO NodePtr
fromNode (Node _ nodeType children) = do
  node <- case nodeType of
            DOCUMENT    -> c_cmark_node_new (#const CMARK_NODE_DOCUMENT)
            THEMATIC_BREAK -> c_cmark_node_new (#const CMARK_NODE_THEMATIC_BREAK)
            PARAGRAPH   -> c_cmark_node_new (#const CMARK_NODE_PARAGRAPH)
            BLOCK_QUOTE -> c_cmark_node_new (#const CMARK_NODE_BLOCK_QUOTE)
            HTML_BLOCK literal -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_HTML_BLOCK)
                     c_cmark_node_set_literal n =<< fromtext literal
                     return n
            CUSTOM_BLOCK onEnter onExit -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_CUSTOM_BLOCK)
                     c_cmark_node_set_on_enter n =<< fromtext onEnter
                     c_cmark_node_set_on_exit  n =<< fromtext onExit
                     return n
            CODE_BLOCK info literal -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_CODE_BLOCK)
                     c_cmark_node_set_literal n =<< fromtext literal
                     c_cmark_node_set_fence_info n =<< fromtext info
                     return n
            LIST attr   -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_LIST)
                     c_cmark_node_set_list_type n $ case listType attr of
                         ORDERED_LIST -> #const CMARK_ORDERED_LIST
                         BULLET_LIST  -> #const CMARK_BULLET_LIST
                     c_cmark_node_set_list_delim n $ case listDelim attr of
                         PERIOD_DELIM -> #const CMARK_PERIOD_DELIM
                         PAREN_DELIM  -> #const CMARK_PAREN_DELIM
                     c_cmark_node_set_list_tight n $ listTight attr
                     c_cmark_node_set_list_start n $ listStart attr
                     return n
            ITEM        -> c_cmark_node_new (#const CMARK_NODE_ITEM)
            HEADING lev  -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_HEADING)
                     c_cmark_node_set_heading_level n lev
                     return n
            EMPH        -> c_cmark_node_new (#const CMARK_NODE_EMPH)
            STRONG      -> c_cmark_node_new (#const CMARK_NODE_STRONG)
            LINK url title -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_LINK)
                     c_cmark_node_set_url n =<< fromtext url
                     c_cmark_node_set_title n =<< fromtext title
                     return n
            IMAGE url title -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_IMAGE)
                     c_cmark_node_set_url n =<< fromtext url
                     c_cmark_node_set_title n =<< fromtext title
                     return n
            TEXT literal -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_TEXT)
                     c_cmark_node_set_literal n =<< fromtext literal
                     return n
            CODE literal -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_CODE)
                     c_cmark_node_set_literal n =<< fromtext literal
                     return n
            HTML_INLINE literal -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_HTML_INLINE)
                     c_cmark_node_set_literal n =<< fromtext literal
                     return n
            CUSTOM_INLINE onEnter onExit -> do
                     n <- c_cmark_node_new (#const CMARK_NODE_CUSTOM_INLINE)
                     c_cmark_node_set_on_enter n =<< fromtext onEnter
                     c_cmark_node_set_on_exit  n =<< fromtext onExit
                     return n
            SOFTBREAK   -> c_cmark_node_new (#const CMARK_NODE_SOFTBREAK)
            LINEBREAK   -> c_cmark_node_new (#const CMARK_NODE_LINEBREAK)
            STRIKETHROUGH -> c_cmark_node_new (fromIntegral . Unsafe.unsafePerformIO $ peek c_CMARK_NODE_STRIKETHROUGH)
            TABLE _       -> error "constructing table not supported"
            TABLE_ROW     -> error "constructing table row not supported"
            TABLE_CELL    -> error "constructing table cell not supported"
  mapM_ (\child -> fromNode child >>= c_cmark_node_append_child node) children
  return node

totext :: CString -> IO Text
totext str
  | str == nullPtr = return empty
  | otherwise      = TF.peekCStringLen (str, c_strlen str)

fromtext :: Text -> IO CString
fromtext t = B.useAsCString (encodeUtf8 t) return

foreign import ccall "string.h strlen"
    c_strlen :: CString -> Int

foreign import ccall "cmark.h cmark_node_new"
    c_cmark_node_new :: Int -> IO NodePtr

foreign import ccall "cmark.h cmark_render_html"
    c_cmark_render_html :: NodePtr -> CInt -> LlistPtr ExtensionPtr -> IO CString

foreign import ccall "cmark.h cmark_render_xml"
    c_cmark_render_xml :: NodePtr -> CInt -> IO CString

foreign import ccall "cmark.h cmark_render_man"
    c_cmark_render_man :: NodePtr -> CInt -> Int -> IO CString

foreign import ccall "cmark.h cmark_render_latex"
    c_cmark_render_latex :: NodePtr -> CInt -> Int -> IO CString

foreign import ccall "cmark.h cmark_render_commonmark"
    c_cmark_render_commonmark :: NodePtr -> CInt -> Int -> IO CString

foreign import ccall "cmark.h cmark_parser_new"
    c_cmark_parser_new :: CInt -> IO ParserPtr

foreign import ccall "cmark.h cmark_parser_feed"
    c_cmark_parser_feed :: ParserPtr -> CString -> Int -> IO ()

foreign import ccall "cmark.h cmark_parser_finish"
    c_cmark_parser_finish :: ParserPtr -> IO NodePtr

foreign import ccall "cmark.h cmark_parser_free"
    c_cmark_parser_free :: ParserPtr -> IO ()

foreign import ccall "cmark.h cmark_node_get_type"
    c_cmark_node_get_type :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_first_child"
    c_cmark_node_first_child :: NodePtr -> IO NodePtr

foreign import ccall "cmark.h cmark_node_next"
    c_cmark_node_next :: NodePtr -> IO NodePtr

foreign import ccall "cmark.h cmark_node_get_literal"
    c_cmark_node_get_literal :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_get_url"
    c_cmark_node_get_url :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_get_title"
    c_cmark_node_get_title :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_get_heading_level"
    c_cmark_node_get_heading_level :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_list_type"
    c_cmark_node_get_list_type :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_list_tight"
    c_cmark_node_get_list_tight :: NodePtr -> IO Bool

foreign import ccall "cmark.h cmark_node_get_list_start"
    c_cmark_node_get_list_start :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_list_delim"
    c_cmark_node_get_list_delim :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_fence_info"
    c_cmark_node_get_fence_info :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_get_start_line"
    c_cmark_node_get_start_line :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_start_column"
    c_cmark_node_get_start_column :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_end_line"
    c_cmark_node_get_end_line :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_end_column"
    c_cmark_node_get_end_column :: NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_get_on_enter"
    c_cmark_node_get_on_enter :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_get_on_exit"
    c_cmark_node_get_on_exit :: NodePtr -> IO CString

foreign import ccall "cmark.h cmark_node_append_child"
    c_cmark_node_append_child :: NodePtr -> NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_set_literal"
    c_cmark_node_set_literal :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_url"
    c_cmark_node_set_url :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_title"
    c_cmark_node_set_title :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_heading_level"
    c_cmark_node_set_heading_level :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_type"
    c_cmark_node_set_list_type :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_tight"
    c_cmark_node_set_list_tight :: NodePtr -> Bool -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_start"
    c_cmark_node_set_list_start :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_delim"
    c_cmark_node_set_list_delim :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_fence_info"
    c_cmark_node_set_fence_info :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_on_enter"
    c_cmark_node_set_on_enter :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_on_exit"
    c_cmark_node_set_on_exit :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h &cmark_node_free"
    c_cmark_node_free :: FunPtr (NodePtr -> IO ())

foreign import ccall "core-extensions.h core_extensions_ensure_registered"
    c_core_extensions_ensure_registered :: IO ()

foreign import ccall "cmark_extension_api.h cmark_find_syntax_extension"
    c_cmark_find_syntax_extension :: CString -> IO ExtensionPtr

foreign import ccall "cmark.h cmark_llist_append"
    c_cmark_llist_append :: MemPtr -> LlistPtr a -> Ptr () -> IO (LlistPtr a)

foreign import ccall "cmark.h cmark_llist_free"
    c_cmark_llist_free :: MemPtr -> LlistPtr a -> IO ()

foreign import ccall "cmark.h &CMARK_DEFAULT_MEM_ALLOCATOR"
    c_CMARK_DEFAULT_MEM_ALLOCATOR :: MemPtr

foreign import ccall "cmark_extension_api.h cmark_parser_attach_syntax_extension"
    c_cmark_parser_attach_syntax_extension :: ParserPtr -> ExtensionPtr -> IO ()

foreign import ccall "strikethrough.h &CMARK_NODE_STRIKETHROUGH"
    c_CMARK_NODE_STRIKETHROUGH :: Ptr CUShort

foreign import ccall "table.h &CMARK_NODE_TABLE"
    c_CMARK_NODE_TABLE :: Ptr CUShort

foreign import ccall "table.h &CMARK_NODE_TABLE_ROW"
    c_CMARK_NODE_TABLE_ROW :: Ptr CUShort

foreign import ccall "table.h &CMARK_NODE_TABLE_CELL"
    c_CMARK_NODE_TABLE_CELL :: Ptr CUShort

foreign import ccall "core-extensions.h cmarkextensions_get_table_columns"
    c_cmarkextensions_get_table_columns :: NodePtr -> IO CUShort

foreign import ccall "core-extensions.h cmarkextensions_get_table_alignments"
    c_cmarkextensions_get_table_alignments :: NodePtr -> IO (Ptr CUChar)
