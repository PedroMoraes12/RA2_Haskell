READ.ME =

# Sistema de Inventário em Haskell

Este projeto implementa um sistema de gerenciamento de inventário via terminal (CLI), desenvolvido como Atividade Avaliativa da disciplina de Programação Lógica e Funcional. O sistema utiliza conceitos de programação funcional pura para lógica de negócios, separando-a das operações de I/O e persistência de dados.

---

## Ambiente de Execução (Online GDB)

O projeto foi desenvolvido e testado para rodar no **Online GDB**.

**https://www.onlinegdb.com/edit/0NwrjIuda**

---


## Instituição 
- Nome da Instituição: PUCPR - Pontifícia Universidade Católica do Paraná.
- Disciplina: Programação Lógica e Funcional.
- Professor: Frank Coelho de Alcantara.

## Alunos do Grupo em Ordem Alfabética
- Aluno 1: Alan Filipe Reginato de França Santos - GitHub: lipeerz.
- Aluno 2: Lucas Ferraz dos Santos - GitHub: lucasferraz122.
- Aluno 3: Pedro Henrique Moraes - GitHub: PedroMoraes12.


---

## Como Compilar e Executar

1.  Acesse o link do Online GDB fornecido acima.
2.  Clique no botão verde **"Run"** (ou pressione F9).
3.  O terminal interativo abrirá na parte inferior da tela.

### Comandos Disponíveis

| Comando | Exemplo de Uso | Descrição |
| :--- | :--- | :--- |
| `add` | `add 001 Teclado 10 Perifericos` | Adiciona um novo item ao inventário. |
| `remove` | `remove 001 5` | Remove uma quantidade do estoque de um item. |
| `list` | `list` | Exibe todos os itens cadastrados. |
| `report errors` | `report errors` | Exibe o log de todas as operações que falharam. |
| `report item` | `report item Teclado` | Exibe o histórico de operações de um item específico. |
| `quit` | `quit` | Salva os dados e encerra o programa. |

---

## Documentação dos Cenários de Teste (Validação Manual)

Abaixo estão os resultados dos testes manuais executados para validar a persistência, a lógica de negócios e o sistema de relatórios, conforme exigido.

### Cenário 1: Persistência de Estado (Sucesso)
**Objetivo:** Verificar se os dados são salvos em disco e recuperados após reiniciar o programa.

1.  **Ação:** Iniciei o programa (base vazia) e adicionei 3 itens.
    ```text
    [+] Tentando carregar Inventario.dat...
    [!] Arquivo nao encontrado ou vazio. Iniciando com inventario vazio.
    [+] Tentando carregar Auditoria.log...
    [!] Arquivo de log nao encontrado ou vazio. Iniciando com logs vazios.
    
    > add 101 Caneta 50 Papelaria
    Item adicionado com sucesso.
    > add 102 Lapis 100 Papelaria
    Item adicionado com sucesso.
    > add 103 Caderno 20 Papelaria
    Item adicionado com sucesso.
    > quit
    Saindo...
    ```
2.  **Ação:** Parei a execução ("Stop") e iniciei novamente ("Run").
    ```text
    [+] Tentando carregar Inventario.dat...
    [!] Inventario carregado com sucesso.
    [+] Tentando carregar Auditoria.log...
    [!] 3 entradas de log carregadas.
    ```
3.  **Ação:** Executei o comando `list` para verificar o estado.
    ```text
    > list
    --- Inventario Atual ---
    Item {itemID = "101", nome = "Caneta", quantidade = 50, categoria = "Papelaria"}
    Item {itemID = "102", nome = "Lapis", quantidade = 100, categoria = "Papelaria"}
    Item {itemID = "103", nome = "Caderno", quantidade = 20, categoria = "Papelaria"}
    ```
**Resultado:** Sucesso. Os 3 itens persistiram entre as execuções.

### Cenário 2: Validação de Lógica (Estoque Insuficiente)
**Objetivo:** Garantir que o sistema impeça a remoção de uma quantidade maior que o estoque atual.

1.  **Ação:** Adicionei um item com 10 unidades (usando o "Monitor" como exemplo).
    ```text
    > add 201 Monitor 10 Eletronicos
    Item adicionado com sucesso.
    ```
2.  **Ação:** Tentei remover 15 unidades (mais do que o disponível).
    ```text
    > remove 201 15
    Erro: Estoque insuficiente.
    ```
    * (Verificação: O programa exibiu uma mensagem de erro clara).
3.  **Ação:** Verifiquei se o estoque foi alterado incorretamente.
    ```text
    > list
    --- Inventario Atual ---
    ...
    Item {itemID = "201", nome = "Monitor", quantidade = 10, categoria = "Eletronicos"}
    ...
    ```
**Resultado:** Sucesso. O erro foi exibido e a quantidade do item "Monitor" permaneceu **10**, provando que a transação falha não alterou o estado.

### Cenário 3: Relatório de Erros
**Objetivo:** Verificar se a falha do Cenário 2 foi registrada no log e aparece no relatório de erros.

1.  **Ação:** Executei o comando de relatório de erros após a falha do Cenário 2.
    ```text
    > report errors
    
    --- Relatorio de Erros ---
    LogEntry {timestamp = 2023-11-14..., acao = Remove, detalhes = "Falha ao remover 15 de 201", status = Falha "Erro: Estoque insuficiente."}
    ```
**Resultado:** Sucesso. A tentativa inválida foi auditada e filtrada corretamente pela função `logsDeErro`.

---

## Estrutura do Código

O código está contido em um único arquivo `main.hs` para compatibilidade com o Online GDB, mas está logicamente dividido nas seções sugeridas no documento do projeto:

1.  **SEÇÃO 1 (Arquiteto de Dados):** Definições dos tipos `Item`, `Inventario`, `AcaoLog`, `StatusLog`, e `LogEntry`.
2.  **SEÇÃO 2 (Lógica Pura):** Funções puras `adicionarItem` e `removerItem`, que retornam `Either` e não realizam IO.
3.  **SEÇÃO 3 (Validação):** Funções puras de relatório (`logsDeErro`, `historicoPorItem`) usando List Comprehension.
4.  **SEÇÃO 4 (I/O e Persistência):** Funções `main`, `appLoop`, `carregarInventario` (com `catch`), `salvarInventario` (`writeFile`) e `salvarLog` (`appendFile`).
