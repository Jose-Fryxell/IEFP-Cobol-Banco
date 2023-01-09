      ******************************************************************
      * Author: JOSE SERRA
      * Date: 22-03-2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAREFAFINAL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ****** DATA & HORA ***********************************************
       01 DATA-ATUAL.
           05 ANO                      PIC 9999.
           05 MES                      PIC 99.
           05 DIA                      PIC 99.
           05 HORA                     PIC 99.
           05 MINUTOS                  PIC 99.
      ****** SALDO *****************************************************
       77 TEMP                         PIC X(10) VALUE SPACES.
       77 TEMP-OPERACAO                PIC S99999999999V99 VALUE 0.
       77 TEMP-OPERACAO-SAIDA          PIC ZZZZZZZZZZ9.99 VALUE SPACES.
       77 SALDO                        PIC S99999999999V99 VALUE 0.
       77 SALDO-SAIDA                  PIC ZZZZZZZZZZ9.99 VALUE SPACES.
      ****** RESPOSTA **************************************************
       77 RESPOSTA                     PIC A.
       77 LINHA                        PIC 99 OCCURS 2 TIMES.
       77 FIX-SOBP-TABELA              PIC 9.

       SCREEN SECTION.
       01 CLS BLANK SCREEN.

       01 JANELA.
           05 LINE 01 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "+------------------------------------------+".
           05 LINE 02 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 03 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "+------------------------------------------+".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 HIGHLIGHT VALUE
           "              Conta Corrente              ".
           05 LINE 04 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 05 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "+------------------------------------------+".
           05 LINE 06 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 07 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
       01 LIMPAR.
           05 LINE 08 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 09 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 10 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 11 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 12 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 13 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "|                                          |".
           05 LINE 14 COL 01 FOREGROUND-COLOR 1 HIGHLIGHT VALUE
           "+------------------------------------------+".
       PROCEDURE DIVISION.
       INICIO.
      ******************************************************************
           MOVE 7 TO LINHA(1).
           MOVE 0 TO FIX-SOBP-TABELA.
           DISPLAY CLS.
           DISPLAY JANELA.
           DISPLAY LIMPAR.
           DISPLAY "Data:" FOREGROUND-COLOR 3 HIGHLIGHT AT 0403.
           DISPLAY "Hora:" FOREGROUND-COLOR 3 HIGHLIGHT AT 0432
           PERFORM ATUALIZAR-HORA.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT 0409.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTOS)
           HIGHLIGHT AT 0438.
           DISPLAY "Bem-vindo!" HIGHLIGHT AT 0603.
      ******************************************************************
           DISPLAY "Por favor digite o saldo inicial:"
           HIGHLIGHT AT 0803.
       LER-SALDO-INICIAL.
           ACCEPT TEMP HIGHLIGHT AT 0903.
           MOVE TEMP TO SALDO.
           EVALUATE TEMP
               WHEN SPACES
               WHEN 0
                   COMPUTE SALDO = 0
               WHEN OTHER
                   IF (SALDO = 0) THEN
                       DISPLAY "Por favor digite numeros."
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       GO LER-SALDO-INICIAL
                   ELSE
                       DISPLAY "                         " AT 1003
                   END-IF
                   CONTINUE
           END-EVALUATE.
           MOVE SALDO TO SALDO-SAIDA.
           PERFORM EXTRATO.
       MENU.
      ****** ATUALIZAR HORA & SALDO ************************************
           PERFORM ATUALIZAR-HORA.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT 0409.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTOS)
           HIGHLIGHT AT 0438.
           PERFORM DISPLAY-SALDO.
      ******************************************************************
           DISPLAY LIMPAR.
           DISPLAY "Por favor digite a letra da operacao que"
           HIGHLIGHT AT 0803.
           DISPLAY "deseja efetuar:" HIGHLIGHT AT 0903.
           DISPLAY "D - Debito" HIGHLIGHT AT 1103.
           DISPLAY "C - Credito" HIGHLIGHT AT 1203.
           DISPLAY "S - Sair" HIGHLIGHT AT 1303.
       LER-RESPOSTA.
           ACCEPT RESPOSTA HIGHLIGHT AT 0919.
           EVALUATE RESPOSTA
               WHEN 'D'
               WHEN 'd'
                   PERFORM DEBITO
               WHEN 'C'
               WHEN 'c'
                   PERFORM CREDITO
               WHEN 'S'
               WHEN 's'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0921
                   GO LER-RESPOSTA
           END-EVALUATE.
           DISPLAY LIMPAR.
           DISPLAY "Deseja repetir o programa ( / )?        "
           HIGHLIGHT AT 0603.
           DISPLAY "S" FOREGROUND-COLOR 2 HIGHLIGHT AT 0630.
           DISPLAY "N" FOREGROUND-COLOR 4 HIGHLIGHT AT 0632.
       REPETIR-PROGRAMA.
           ACCEPT RESPOSTA HIGHLIGHT AT 0636.
           EVALUATE RESPOSTA
               WHEN 'S'
               WHEN 's'
                   GO INICIO
               WHEN 'N'
               WHEN 'n'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Por favor insira 'S' ou 'N'."
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0803
                   GO REPETIR-PROGRAMA
           END-EVALUATE.
           STOP RUN.
       DISPLAY-SALDO.
           DISPLAY "                              " AT 0613.
           DISPLAY "Eur" FOREGROUND-COLOR 6 HIGHLIGHT AT 0640.
           IF (SALDO < 0) THEN
               DISPLAY SALDO-SAIDA
               FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
           ELSE
               IF (SALDO > 0)
                   DISPLAY SALDO-SAIDA
                   FOREGROUND-COLOR 2 HIGHLIGHT AT 0625
               ELSE
                   DISPLAY SALDO-SAIDA HIGHLIGHT AT 0625
               END-IF
           END-IF.
           EVALUATE SALDO
               WHEN > 9999999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0618
               WHEN > 999999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0619
               WHEN > 99999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0620
               WHEN > 9999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0621
               WHEN > 999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0622
               WHEN > 99999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0623
               WHEN > 9999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0624
               WHEN > 999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0625
               WHEN > 99.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0626
               WHEN > 9.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0627
               WHEN < -9999999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0617
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0624
               WHEN < -999999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0618
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0625
               WHEN < -99999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0619
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0626
               WHEN < -9999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0620
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0627
               WHEN < -999999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0621
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0628
               WHEN < -99999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0622
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0629
               WHEN < -9999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0623
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0630
               WHEN < -999.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0624
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0631
               WHEN < -99.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0625
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0632
               WHEN < -9.99
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0626
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0633
               WHEN >= 0
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0628
                   DISPLAY " "
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0634
               WHEN OTHER
                   DISPLAY "Saldo:"
                   FOREGROUND-COLOR 3 HIGHLIGHT AT 0627
                   DISPLAY "-"
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0634
           END-EVALUATE.
       DEBITO.
           DISPLAY LIMPAR.
           DISPLAY "Por favor digite a despesa:"
           HIGHLIGHT AT 0803.
           PERFORM LER-OPERACAO.
      ******************************************************************
           COMPUTE SALDO = SALDO - TEMP-OPERACAO.
           MOVE SALDO TO SALDO-SAIDA.
           PERFORM EXTENDER-EXTRATO.
           GO MENU.
      ******************************************************************
       CREDITO.
           DISPLAY LIMPAR.
           DISPLAY "Por favor digite o credito:"
           HIGHLIGHT AT 0803.
           PERFORM LER-OPERACAO.
      ******************************************************************
           COMPUTE SALDO = SALDO + TEMP-OPERACAO.
           MOVE SALDO TO SALDO-SAIDA.
           PERFORM EXTENDER-EXTRATO.
           GO MENU.
      ******************************************************************
       LER-OPERACAO.
           DISPLAY "Insira 'V' para voltar."
           HIGHLIGHT AT 1303.
           ACCEPT TEMP HIGHLIGHT AT 0903.
           MOVE TEMP TO TEMP-OPERACAO.
           EVALUATE TEMP
               WHEN "V"
               WHEN "v"
                   GO MENU
               WHEN SPACES
               WHEN 0
                   COMPUTE TEMP-OPERACAO = 0
               WHEN OTHER
                   IF (TEMP-OPERACAO = 0) THEN
                       DISPLAY "Por favor digite numeros.           "
                       FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
                       GO LER-OPERACAO
                   ELSE
                       DISPLAY "                         " AT 1003
                   END-IF
                   CONTINUE
           END-EVALUATE.
           IF (TEMP-OPERACAO <= 0) THEN
               DISPLAY "Por favor digite um numero positivo."
               FOREGROUND-COLOR 4 HIGHLIGHT AT 1003
               GO LER-OPERACAO
           ELSE
               DISPLAY "                         " AT 1003
           END-IF.
       ATUALIZAR-HORA.
           MOVE FUNCTION CURRENT-DATE TO DATA-ATUAL.
       EXTRATO.
           DISPLAY FUNCTION CONCATENATE("+----------------------------",
           "-------------------------------------+")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0148.
           DISPLAY FUNCTION CONCATENATE("|                            ",
           "                                     |")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0248.
           DISPLAY FUNCTION CONCATENATE("+------------+-------+-------",
           "---------------+---------------------+")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0348.
           DISPLAY FUNCTION CONCATENATE("|            |       |       ",
           "               |                     |")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0448.
           DISPLAY FUNCTION CONCATENATE("+------------+-------+-------",
           "---------------+---------------------+")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0548.
           DISPLAY FUNCTION CONCATENATE("|            |       |       ",
           "               |                     |")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0648.
           DISPLAY FUNCTION CONCATENATE("+------------+-------+-------",
           "---------------+---------------------+")
           FOREGROUND-COLOR 1 HIGHLIGHT AT 0748.
           DISPLAY "Extrato" FOREGROUND-COLOR 3 HIGHLIGHT AT 0278.
           DISPLAY "Data" FOREGROUND-COLOR 3 HIGHLIGHT AT 0453.
           DISPLAY "Hora" FOREGROUND-COLOR 3 HIGHLIGHT AT 0463.
           DISPLAY "Operacao" FOREGROUND-COLOR 3 HIGHLIGHT AT 0477.
           DISPLAY "Saldo" FOREGROUND-COLOR 3 HIGHLIGHT AT 004101.
      ******************************************************************
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT 0650.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTOS)
           HIGHLIGHT AT 0663.
           DISPLAY "Saldo Inicial" HIGHLIGHT AT 0678.
           IF (SALDO > 0) THEN
               DISPLAY SALDO-SAIDA
               FOREGROUND-COLOR 2 HIGHLIGHT AT 0695
           ELSE
               IF (SALDO < 0) THEN
                   DISPLAY SALDO-SAIDA
                   FOREGROUND-COLOR 4 HIGHLIGHT AT 0695
                   EVALUATE SALDO
                   WHEN < -9999999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 94
                   WHEN < -999999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 95
                   WHEN < -99999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 96
                   WHEN < -9999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 97
                   WHEN < -999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 98
                   WHEN < -99999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 99
                   WHEN < -9999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 100
                   WHEN < -999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 101
                   WHEN < -99.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 102
                   WHEN < -9.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 103
                   WHEN OTHER
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE 6 COL 104
               END-EVALUATE
               ELSE
                   DISPLAY SALDO-SAIDA HIGHLIGHT AT 0695
               END-IF
           END-IF.
           DISPLAY "Eur" FOREGROUND-COLOR 6 HIGHLIGHT AT 006110.
       EXTENDER-EXTRATO.
           IF (LINHA(1) = 28) THEN
               IF (FIX-SOBP-TABELA = 0) THEN
                   MOVE 1 TO FIX-SOBP-TABELA
               END-IF
               MOVE 6 TO LINHA(1)
           END-IF.
           IF (FIX-SOBP-TABELA = 1) THEN
               IF (LINHA(1) = 26) THEN
                   DISPLAY FUNCTION CONCATENATE("                     ",
                   "                                              ")
                   AT 2848
               END-IF
           END-IF.
           COMPUTE LINHA(2) = LINHA(1) + 1.
           DISPLAY FUNCTION CONCATENATE("|            |       |       ",
           "               |                     |")
           FOREGROUND-COLOR 1 HIGHLIGHT AT LINE LINHA(1) COL 48.
           DISPLAY FUNCTION CONCATENATE("+------------+-------+-------",
           "---------------+---------------------+")
           FOREGROUND-COLOR 1 HIGHLIGHT AT LINE LINHA(2) COL 48.
           DISPLAY FUNCTION CONCATENATE(DIA,"-",MES,"-",ANO)
           HIGHLIGHT AT LINE LINHA(1) COL 50.
           DISPLAY FUNCTION CONCATENATE(HORA,":",MINUTOS)
           HIGHLIGHT AT LINE LINHA(1) COL 63.
           IF (SALDO < 0) THEN
               DISPLAY SALDO-SAIDA
               FOREGROUND-COLOR 4 HIGHLIGHT LINE LINHA(1) COL 95
               EVALUATE SALDO
                   WHEN < -9999999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 94
                   WHEN < -999999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 95
                   WHEN < -99999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 96
                   WHEN < -9999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 97
                   WHEN < -999999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 98
                   WHEN < -99999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 99
                   WHEN < -9999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 100
                   WHEN < -999.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 101
                   WHEN < -99.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 102
                   WHEN < -9.99
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 103
                   WHEN OTHER
                       DISPLAY "-"
                       FOREGROUND-COLOR 4 HIGHLIGHT
                       LINE LINHA(1) COL 104
               END-EVALUATE
           ELSE
               IF (SALDO > 0)
                   DISPLAY SALDO-SAIDA
                   FOREGROUND-COLOR 2 HIGHLIGHT LINE LINHA(1) COL 95
               ELSE
                   DISPLAY SALDO-SAIDA HIGHLIGHT LINE LINHA(1) COL 95
               END-IF
           END-IF.
           MOVE TEMP-OPERACAO TO TEMP-OPERACAO-SAIDA.
           EVALUATE RESPOSTA
               WHEN "D"
               WHEN "d"
                   DISPLAY TEMP-OPERACAO-SAIDA LINE LINHA(1) COL 73
                   FOREGROUND-COLOR 4 HIGHLIGHT
                   EVALUATE TEMP-OPERACAO
                       WHEN > 9999999999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 71
                       WHEN > 999999999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 72
                       WHEN > 99999999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 73
                       WHEN > 9999999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 74
                       WHEN > 999999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 75
                       WHEN > 99999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 76
                       WHEN > 9999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 77
                       WHEN > 999.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 78
                       WHEN > 99.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 79
                       WHEN > 9.99
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 80
                       WHEN OTHER
                           DISPLAY "-"
                           FOREGROUND-COLOR 4 HIGHLIGHT
                           LINE LINHA(1) COL 81
                   END-EVALUATE
               WHEN "C"
               WHEN "c"
                   DISPLAY TEMP-OPERACAO-SAIDA LINE LINHA(1) COL 73
                   FOREGROUND-COLOR 2 HIGHLIGHT
                   EVALUATE TEMP-OPERACAO
                       WHEN > 9999999999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 71
                       WHEN > 999999999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 72
                       WHEN > 99999999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 73
                       WHEN > 9999999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 74
                       WHEN > 999999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 75
                       WHEN > 99999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 76
                       WHEN > 9999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 77
                       WHEN > 999.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 78
                       WHEN > 99.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 79
                       WHEN > 9.99
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 80
                       WHEN OTHER
                           DISPLAY "+"
                           FOREGROUND-COLOR 2 HIGHLIGHT
                           LINE LINHA(1) COL 81
                   END-EVALUATE
           END-EVALUATE.
           DISPLAY "Eur" FOREGROUND-COLOR 6
           HIGHLIGHT LINE LINHA(1) COL 88.
           DISPLAY "Eur" FOREGROUND-COLOR 6
           HIGHLIGHT LINE LINHA(1) COL 110.
           ADD 1 TO LINHA(1).
       END PROGRAM TAREFAFINAL.
