# RA2-6

## Informações:
<br>
 Instituição: PUCPR<br>
 Matéria: Programação Lógica e Funcional<br>
 Professor: Frank de Alcantara<br>
 Curso: Ciência da Computação<br>

## 🧑‍🎓 ALUNOS:
<br>

 **Alisson Daledone**
 <br>
 **Geandro Marques**
 <br>
 **Juliano Teles**

## 📖 COMO USAR:
  **Adicionar Item**<br>
  para adicionar um item digite no terminal "add 'id' 'nome do item' 'quantidade' 'categoria/tipo' "<br>
  EX: "add 1 bandeiraDoVasco 12 esportivos"

  <br>

  **Atualizar Quantidade**<br>
  Para atualizar a quantidade de um item (seja aumentando ou diminuindo) digite no terminal "update 'id' 'quantidade'"<br>
  EX aumentar quantidade: "update 1 10" (adiciona 10 quantidades do item com código 1)<br>
  EX diminuir quantidade: "update 1 -12" (tira 12 quantidades do item com código 1)<br>

  **Remover Item**
  Para remover completamente um item do inventário digite no terminal "remover 'id"<br>
  EX: "remover 1"<br>

## CENARIOS:

  **Cenario 1 Persistência de Estado (Sucesso)**<br>
  Iniciar o programa (sem arquivos de dados).<br>
  <img width="1480" height="302" alt="image" src="https://github.com/user-attachments/assets/e334a8b4-1404-408c-993d-e2c0fe36be2a" />
  <br>
  Adicionar 3 itens.<br>
  <img width="1448" height="197" alt="image" src="https://github.com/user-attachments/assets/bf5a2180-8c0e-4b8b-a518-92ef4df637e0" />
  <br>
  Fechar o programa.<br>
  <img width="383" height="115" alt="image" src="https://github.com/user-attachments/assets/93f12783-fa5b-486b-bf09-a2ae37dabf50" />
  <br>
  Verificar se os arquivos Inventario.dat e Auditoria.log foram criados.<br>
  <img width="1434" height="120" alt="image" src="https://github.com/user-attachments/assets/abb1433b-3ed1-4480-8fb5-a3d678df9b45" /><br>
  <img width="1305" height="63" alt="image" src="https://github.com/user-attachments/assets/45a66283-45af-40f2-bf2a-cf8dec3352c0" /><br>
  Reiniciar o programa.<br>
  Executar um comando de "listar" (a ser criado) ou verificar se o estado carregado em
  memória contém os 3 itens.<br>
  <img width="763" height="100" alt="image" src="https://github.com/user-attachments/assets/f9415049-e7f0-416e-8dc7-49a07ec2cea7" /><br>
  <br>

  **Cenário 2: Erro de Lógica (Estoque Insuficiente)**<br>
  Adicionar um item com 10 unidades (ex: "teclado").<br>
  Tentar remover 15 unidades desse item.<br>
  Verificar se o programa exibiu uma mensagem de erro clara.<br>
  <img width="1280" height="124" alt="image" src="https://github.com/user-attachments/assets/80cf63ed-bd71-4b4b-9999-cdbf8b6c5c17" /><br>
  Verificar se o Inventario.dat (e o estado em memória) ainda mostra 10 unidades.<br>
  <img width="1090" height="35" alt="image" src="https://github.com/user-attachments/assets/2bb840a2-d1b8-45f0-bb66-674b55f2fcfa" /><br>
  Verificar se o Auditoria.log contém uma LogEntry com StatusLog (Falha ...).<br>
  <img width="1280" height="54" alt="image" src="https://github.com/user-attachments/assets/aaa3e76e-dbac-47a7-bf7f-272b2407763a" /><br>
  <br>

  **Cenário 3: Geração de Relatório de Erros**<br>
  Após executar o Cenário 2, executar o comando report.<br>
  Verificar se o relatório gerado (especificamente pela função logsDeErro) exibe a
  entrada de log referente à falha registrada no Cenário 2 (a tentativa de remover
  estoque insuficiente)<br>
  <img width="1258" height="174" alt="image" src="https://github.com/user-attachments/assets/4d36e9b1-23d8-4b92-9fb7-577d7c22884e" />

  

  

  

  

  



  

  

  
  

  

  
  
  
  
