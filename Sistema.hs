import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.IORef
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.List (isInfixOf)
import Control.Exception
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

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

-- função de remover
removeItem :: UTCTime -> Item -> Inventario -> Either LogEntry ResultadoOperacao
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
                    {timestamp = time
                    , acao = Remove
                    , detalhes = "Falha ao remover. O item não existe: " ++ itemID itemRemovido
                    , status = Falha "O item não existe"
                    }
            in Left logEntry


-- funcão de adicionar
addItem :: UTCTime -> Item -> Inventario -> Either LogEntry ResultadoOperacao
addItem time itemAdicionado (Inventario mapa) =
    case Map.lookup (itemID itemAdicionado) mapa of
        Just _ ->
            let logEntry = LogEntry
                    {timestamp = time
                    , acao = Add
                    , detalhes = "Falha ao adicionar, ID duplicado, Item: " ++ itemID itemAdicionado
                    , status = Falha "item Duplicado"
                    }
            in Left logEntry
        
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

-- fução de atualizar           
updateQty :: UTCTime -> String -> Int -> Inventario -> Either LogEntry ResultadoOperacao
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
                    in Left logEntry

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
            in Left logEntry

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
            


loadInventario :: IO Inventario
loadInventario = do
  let arquivo = "inventario.dat"
  existe <- doesFileExist arquivo
  if not existe
        then writeFile arquivo ""  
        else return ()
  catch
    (do content <- readFile arquivo
        case readMaybe content of
          Just inv -> return inv
          Nothing  -> return (Inventario Map.empty))
    (\(_ :: IOException) -> return (Inventario Map.empty))
    
    

loadLogs :: IO [LogEntry]
loadLogs = do
  let arquivo = "Auditoria.log"
  existe <- doesFileExist arquivo
  if not existe
        then writeFile arquivo "" 
        else return ()
  catch
    (do
          content <- readFile arquivo
          let linhas = lines content
              logs = mapMaybe readMaybe linhas 
          return logs
      )
      (\(_ :: IOException) -> return [])

salvarInventario :: Inventario -> IO()
salvarInventario inv = writeFile "inventario.dat" (show inv)

appendLog :: LogEntry -> IO ()
appendLog l = appendFile "Auditoria.log" (show l ++ "\n")
            
            
adicionarLog :: IORef [LogEntry] -> LogEntry -> IO ()
adicionarLog logRef novoLog = do
    logsAtuais <- readIORef logRef
    writeIORef logRef (logsAtuais ++ [novoLog])            
            
            
            
main :: IO ()
main = do
    condicao <- newIORef True
    
    inventario <- loadInventario
    logs <- loadLogs
    
    inventarioRef <- newIORef inventario
    logsRef <- newIORef logs
    let aplicacao = do
            bandeira <- readIORef condicao
            if bandeira
                then do
                    putStr "> "
                    hFlush stdout
                    input <- getLine
                    let tokens = words input
                    case tokens of
                                ("add":id:nomeI:qtd:cat:_) -> do 
                                    let qtdInt = read qtd :: Int
                                    let item = Item {
                                        itemID = id,
                                        nome = nomeI,
                                        quantidade = qtdInt,
                                        categoria = cat
                                    }
                                    invAtual <- readIORef inventarioRef
                                    logAtuak <- readIORef logsRef
                                    time <- getCurrentTime
                                    case addItem time item invAtual of
                                        Right (novoInv, logEntry) -> do
                                            writeIORef inventarioRef novoInv
                                            salvarInventario novoInv
                                            adicionarLog logsRef logEntry
                                            appendLog logEntry
                                            print "Item adicionado"
                                            aplicacao
                                        Left err -> do
                                            appendLog err
                                            adicionarLog logsRef err
                                            print $ err
                                            aplicacao
                                ("remover":id:_)       -> do 
                                    invAtual <- readIORef inventarioRef
                                    
                                    case Map.lookup id (itens invAtual) of
                                        Just item -> do 
                                            time <- getCurrentTime
                                            case removeItem time item invAtual of
                                                Right (novoInv, logEntry) -> do
                                                    writeIORef inventarioRef novoInv
                                                    salvarInventario novoInv
                                                    appendLog logEntry
                                                    adicionarLog logsRef logEntry
                                                    print "Item removido"
                                                    aplicacao
                                                Left err -> do
                                                    appendLog err
                                                    adicionarLog logsRef err
                                                    print $ err
                                                    aplicacao
                                        
                                        Nothing   -> do 
                                            putStrLn "Item não encontrado"
                                            aplicacao
                                ("update":id:qtd:_)       -> do
                                    invAtual <- readIORef inventarioRef
                                    case Map.lookup id (itens invAtual) of
                                        Just item -> do
                                            let qtdInt = read qtd :: Int
                                            time <- getCurrentTime
                                            case updateQty time id (qtdInt) invAtual of
                                                    Right (novoInv, logEntry) -> do
                                                        writeIORef inventarioRef novoInv
                                                        salvarInventario novoInv
                                                        appendLog logEntry
                                                        adicionarLog logsRef logEntry
                                                        print "Item Atualizado"
                                                        aplicacao
                                                    Left err -> do
                                                        appendLog err
                                                        adicionarLog logsRef err
                                                        print $ err
                                                        aplicacao
                                        
                                        Nothing   -> do 
                                            putStrLn "Item não encontrado"
                                            aplicacao
                                ["listar"]                -> do
                                    invAtual <- readIORef inventarioRef
                                    mapM_ print (Map.elems (itens invAtual))
                                    aplicacao
                                ["report"]                -> do 
                                    logsAtuais <- readIORef logsRef
                                    putStrLn $ report logsAtuais
                                    aplicacao
                                ["exit"]                  -> do 
                                    print "encerrando..."
                                    writeIORef condicao False
                                _ -> do
                                    putStrLn "Comando inválido"
                                    aplicacao
                    
                else
                    return ()
    aplicacao     
