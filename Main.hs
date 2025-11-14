{- 
  Projeto: Sistema de Inventário em Haskell
  Disciplina: Programação Lógica e Funcional
  Professor: Frank Coelho de Alcantara
  Aluno 1: Alan Filipe Reginato de França Santos
  Aluno 2: Lucas Ferraz dos Santos
  Aluno 3: Pedro Henrique Moraes
-}

-- Modulos necessarios
import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import System.IO (IOMode(..), openFile, hClose, hGetContents, hPutStr, appendFile, writeFile, readFile)
import Text.Read (readMaybe)
import Data.List (isInfixOf)

-- SECAO 1: Definicoes de Tipos (Aluno 1: Arquiteto de Dados)

data Item = Item {
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
} deriving (Show, Read, Eq)

type Inventario = M.Map String Item

data AcaoLog = Add | Remove | Update | QueryFail
    deriving (Show, Read, Eq)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read, Eq)

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhes :: String,
    status :: StatusLog
} deriving (Show, Read, Eq)


-- SECAO 2: Logica de Negocio Pura (Aluno 2: Logica Pura)

type ResultadoOperacao = (Inventario, LogEntry)

adicionarItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
adicionarItem time id nome qtd cat inv
    | M.member id inv = Left "Erro: Item com este ID ja existe."
    | qtd < 0 = Left "Erro: Quantidade nao pode ser negativa."
    | otherwise =
        let novoItem = Item id nome qtd cat
            novoInv = M.insert id novoItem inv
            logEntry = LogEntry time Add ("Adicionado: " ++ nome) Sucesso
        in Right (novoInv, logEntry)

removerItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removerItem time id qtdRemover inv =
    case M.lookup id inv of
        Nothing -> 
            Left "Erro: Item nao encontrado."
        
        Just item ->
            if quantidade item < qtdRemover then
                Left "Erro: Estoque insuficiente."
            else
                let novaQtd = quantidade item - qtdRemover
                    itemAtualizado = item { quantidade = novaQtd }
                    novoInv = M.insert id itemAtualizado inv
                    logEntry = LogEntry time Remove ("Removido " ++ show qtdRemover ++ " de " ++ nome item) Sucesso
                in Right (novoInv, logEntry)

logFalhaParser :: UTCTime -> String -> LogEntry
logFalhaParser time cmd = LogEntry time QueryFail ("Comando invalido: " ++ cmd) (Falha "Erro de parser")

-- SECAO 3: Funcoes de Relatorio Puras (Aluno 4: Validacao)

ehErro :: StatusLog -> Bool
ehErro (Falha _) = True
ehErro _         = False

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro listaLogs = [ x | x <- listaLogs, ehErro (status x) ]

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem nomeItem listaLogs = [ x | x <- listaLogs, nomeItem `isInfixOf` detalhes x ]


-- SECAO 4: Modulo de I/O e Persistencia (Aluno 3: I/O)

invFile :: FilePath
invFile = "Inventario.dat"

logFile :: FilePath
logFile = "Auditoria.log"

safeReadFile :: FilePath -> IO String
safeReadFile path = catch (readFile path) handleErr
    where 
        handleErr :: IOException -> IO String
        handleErr e 
            | isDoesNotExistError e = return "" 
            | otherwise = ioError e

carregarInventario :: IO Inventario
carregarInventario = do
    putStrLn "[+] Tentando carregar Inventario.dat..."
    conteudo <- safeReadFile invFile
    if null conteudo then do
        putStrLn "[!] Arquivo nao encontrado ou vazio. Iniciando com inventario vazio."
        return M.empty
    else
        case readMaybe conteudo :: Maybe Inventario of
            Nothing -> do
                putStrLn "[!] ERRO: Nao foi possivel desserializar Inventario.dat. Iniciando com inventario vazio."
                return M.empty
            Just inv -> do
                putStrLn "[!] Inventario carregado com sucesso."
                return inv

parsearLogs :: [String] -> [LogEntry]
parsearLogs [] = []
parsearLogs (x:xs) = 
    case readMaybe x :: Maybe LogEntry of
        Just log -> log : parsearLogs xs
        Nothing  -> parsearLogs xs

carregarLogs :: IO [LogEntry]
carregarLogs = do
    putStrLn "[+] Tentando carregar Auditoria.log..."
    conteudo <- safeReadFile logFile
    if null conteudo then do
        putStrLn "[!] Arquivo de log nao encontrado ou vazio. Iniciando com logs vazios."
        return []
    else do
        let linhas = lines conteudo
            logs = parsearLogs linhas
        putStrLn ("[!] " ++ show (length logs) ++ " entradas de log carregadas.")
        return logs

salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile invFile (show inv)

salvarLog :: LogEntry -> IO ()
salvarLog logEntry = appendFile logFile (show logEntry ++ "\n")

main :: IO ()
main = do
    inventario <- carregarInventario
    logs <- carregarLogs
    
    putStrLn "\n--- Sistema de Gerenciamento de Inventario ---"
    putStrLn "Comandos disponiveis:"
    putStrLn "  add [id] [nome] [qtd] [cat]  (Ex: add 001 Teclado 10 Informatica)"
    putStrLn "  remove [id] [qtd]            (Ex: remove 001 5)"
    putStrLn "  list                           (Lista todos os itens no inventario)"
    putStrLn "  report errors                  (Mostra todos os logs de erro)"
    putStrLn "  report item [nome]           (Mostra historico de um item)"
    putStrLn "  quit                           (Sair do programa)"
    putStrLn "-------------------------------------------------"

    appLoop inventario logs

appLoop :: Inventario -> [LogEntry] -> IO ()
appLoop inventario logs = do
    putStr "\n> "
    linha <- getLine
    time <- getCurrentTime
    
    case words linha of
        ["quit"] -> putStrLn "Saindo..."
        
        ["list"] -> do
            putStrLn "\n--- Inventario Atual ---"
            if M.null inventario
                then putStrLn "(Vazio)"
                else mapM_ (print . snd) (M.toList inventario)
            appLoop inventario logs

        ["report", "errors"] -> do
            putStrLn "\n--- Relatorio de Erros ---"
            let erros = logsDeErro logs
            if null erros
                then putStrLn "(Nenhum erro registrado)"
                else mapM_ print erros
            appLoop inventario logs

        ["report", "item", nome] -> do
            putStrLn ("\n--- Historico do Item: " ++ nome ++ " ---")
            let historico = historicoPorItem nome logs
            if null historico
                then putStrLn "(Nenhum registro para este item)"
                else mapM_ print historico
            appLoop inventario logs

        ["add", id, nome, sqtd, cat] -> do
            case readMaybe sqtd :: Maybe Int of
                Nothing -> do
                    putStrLn "Erro: Quantidade invalida. Deve ser um numero."
                    let logEntry = logFalhaParser time linha
                    salvarLog logEntry
                    appLoop inventario (logs ++ [logEntry])
                Just qtd -> 
                    case adicionarItem time id nome qtd cat inventario of
                        Left erro -> do
                            putStrLn erro
                            let logEntry = LogEntry time Add ("Falha: " ++ erro) (Falha erro)
                            salvarLog logEntry
                            appLoop inventario (logs ++ [logEntry])

                        Right (novoInv, logEntry) -> do
                            putStrLn "Item adicionado com sucesso."
                            salvarInventario novoInv
                            salvarLog logEntry
                            appLoop novoInv (logs ++ [logEntry])

        ["remove", id, sqtd] -> do
            case readMaybe sqtd :: Maybe Int of
                Nothing -> do
                    putStrLn "Erro: Quantidade invalida. Deve ser um numero."
                    let logEntry = logFalhaParser time linha
                    salvarLog logEntry
                    appLoop inventario (logs ++ [logEntry])
                Just qtd ->
                    case removerItem time id qtd inventario of
                        Left erro -> do
                            putStrLn erro
                            let statusFalha = Falha erro
                            let acaoLog = if erro == "Erro: Item nao encontrado." then QueryFail else Remove
                            let logEntry = LogEntry time acaoLog ("Falha ao remover " ++ show qtd ++ " de " ++ id) statusFalha
                            salvarLog logEntry
                            appLoop inventario (logs ++ [logEntry])
                        
                        Right (novoInv, logEntry) -> do
                            putStrLn "Estoque atualizado com sucesso."
                            salvarInventario novoInv
                            salvarLog logEntry
                            appLoop novoInv (logs ++ [logEntry])

        _ -> do
            putStrLn "Comando invalido. Tente novamente."
            let logEntry = logFalhaParser time linha
            salvarLog logEntry
            appLoop inventario (logs ++ [logEntry])
