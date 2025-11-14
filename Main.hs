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

