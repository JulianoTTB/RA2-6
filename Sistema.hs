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

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time itemID quantidadepAtualizar (Inventario mapa) =
    case Map.lookup itemID mapa of
        Just itemAtual ->
            let novaQuantidade = quantidade itemAtual + quantidadepAtualizar
            in if novaQuantidade < 0
                then
                    let logEntry = LogEntry
                            { timestamp = time
                            , acao = Update
                            , detalhes = "Falha ao atualizar, o estoque ficou negativo, item: " ++ itemID
                            , status = Falha "estoque negativo"
                            }
                    in Left "estoque negativo"

                else
                    let itemAtualizado = itemAtual { quantidade = novaQuantidade }
                        novoMapa = Map.insert itemID itemAtualizado mapa
                        novoInventario = Inventario novoMapa
                        logEntry = LogEntry
                            { timestamp = time
                            , acao = Update
                            , detalhes = "Quantidade atualizada! item: " ++ itemID
                            , status = Sucesso
                            }
                    in Right (novoInventario, logEntry)

        Nothing ->
            let logEntry = LogEntry
                    { timestamp = time
                    , acao = Update
                    , detalhes = "Falha ao atualizar, o item não existe, item: " ++ itemID
                    , status = Falha "Item não encontrado"
                    }
            in Left "item não existe"

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter erroExiste
  where
    erroExiste (LogEntry _ _ _ (Falha _)) = True
    erroExiste _ = False

formatarLog :: LogEntry -> String
formatarLog (LogEntry time acao detalhes status) =
    show time ++ " | " ++ show acao ++ " | " ++ detalhes ++ " | " ++ show status

report :: [LogEntry] -> String
report logs =
    let erros = logsDeErro(logs)
        totalErros = length erros
        totalLogs = length logs
    in
        "-===RELATÓRIO===-\n\n" ++
        "Total de operações: " ++ show totalLogs ++ "\n" ++
        "Total de erros: " ++ show totalErros ++ "\n" ++
        "Erros registrados:\n" ++
        unlines (map formatarLog erros)



historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs =
    filter idCorrespondente logs
   where
    idCorrespondente logEntry =
        itemId `isInfixOf` detalhes logEntry
            
