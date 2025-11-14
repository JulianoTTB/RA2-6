import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import Data.List (isInfixOf)

data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read)

data Inventario = Inventario {
    itens :: Map.Map String Item
} deriving (Show, Read)

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read)

type ResultadoOperacao = (Inventario, LogEntry)

removeItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
removeItem time itemRemovido (Inventario mapa) =
    case Map.lookup (itemID itemRemovido) mapa of
        Just _ ->
            let novoMapa = Map.delete (itemID itemRemovido) mapa
                novoInventario = Inventario novoMapa
                logEntry = LogEntry
                    { timestamp = time
                    , acao = Remove
                    , detalhes = "Item removido: " ++ itemID itemRemovido
                    , status = Sucesso
                    }
            in Right (novoInventario, logEntry)

        Nothing ->
            let logEntry = LogEntry
                    { timestamp = time
                    , acao = Remove
                    , detalhes = "Falha ao remover. O item não existe: " ++ itemID itemRemovido
                    , status = Falha "O item não existe"
                    }
            in Left "O item não existe"

-- funcão de adicionar
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time itemAdicionado (Inventario mapa) =
    case Map.lookup (itemID itemAdicionado) mapa of
        Just _ ->
            let logEntry = LogEntry
                    {timestamp = time
                    , acao = Add
                    , detalhes = "Falha ao adicionar, ID duplicado, Item: " ++ itemID itemAdicionado
                    , status = Falha "item Duplicado"
                    }
            in Left "erro item duplicado"
        
        Nothing ->
            let novoMapa = Map.insert (itemID itemAdicionado) itemAdicionado mapa
                novoInventario = Inventario novoMapa
                logEntry = LogEntry
                    { timestamp = time
                    , acao = Add
                    , detalhes = "Item adicionado com sucesso! Item: " ++ itemID itemAdicionado
                    , status = Sucesso
                    }
            in Right (novoInventario, logEntry)



historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs =
    filter idCorrespondente logs
   where
    idCorrespondente logEntry =
        itemId `isInfixOf` detalhes logEntry
            
