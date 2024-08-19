(defun legenda-linhas (tamanho_das_linhas 
                       espacamento_das_linhas 
                       espacamento_de_categorias 
                       espacamento_de_textos
                       ponto_de_insercao 
                       / 
                       posicao_x_inicial 
                       posicao_x_final 
                       posicao_x_texto 
                       posicao_y_atual
                       layer_ja_adicionado
                       linhas
                       vlaobj
                       layer
                       quantidade
                       )
  
  (setq posicao_x_inicial (car ponto_de_insercao))
  (setq posicao_x_final (+ posicao_x_inicial tamanho_das_linhas))
  
  (setq posicao_x_texto (+ posicao_x_final espacamento_de_textos))
  
  (setq posicao_y_atual (cadr ponto_de_insercao))
  
  (setq layer_ja_adicionado (list))
  
  (if (setq linhas (ssget "a" (list (cons 0 "*line"))))  
    (progn
      (repeat (setq quantidade (sslength linhas))
        (setq vlaobj (vlax-ename->vla-object (ssname linhas (setq quantidade (1- quantidade)))))
        
        (setq layer (vla-get-layer vlaobj))
        
        (if (not (member layer layer_ja_adicionado))
          (progn
            (setvar 'clayer layer) ; atualiza layer atual

            (vla-addline model_space (vlax-3d-point posicao_x_inicial posicao_y_atual 0) (vlax-3d-point posicao_x_final posicao_y_atual 0))
            (vla-addtext model_space layer (vlax-3d-point posicao_x_texto posicao_y_atual 0) 3)
            
            (setq posicao_y_atual (- posicao_y_atual espacamento_das_linhas))

            (setq layer_ja_adicionado (cons layer layer_ja_adicionado))
          )
        )
      )
      
      (setq posicao_y_atual (- posicao_y_atual espacamento_de_categorias))
    )
      
  )
  
  (list (nth 0 ponto_de_insercao) posicao_y_atual (nth 2 ponto_de_insercao))
  
)

(defun legenda-blocos (tamanho_das_linhas 
                       espacamento_dos_retangulos 
                       espacamento_de_categorias 
                       espacamento_de_textos 
                       ponto_de_insercao
                       /
                       posicao_x_inicial
                       posicao_x_texto
                       posicao_y_atual
                       layer_ja_adicionado
                       blocos
                       quantidade
                       vlaobj
                       layer
                       )


  (setq posicao_x_inicial (car ponto_de_insercao))
  (setq posicao_x_texto (+ (+ posicao_x_inicial tamanho_das_linhas) espacamento_de_textos))
  (setq posicao_y_atual (cadr ponto_de_insercao))
  
  (setq layer_ja_adicionado (list))  
  
  (if (setq blocos (ssget "a" '(0 . "INSERT")))
    (progn
      (repeat (setq quantidade (sslength blocos))
        (setq vlaobj (vlax-ename->vla-object (ssname blocos (setq quantidade (1- quantidade)))))

        (setq layer (vla-get-layer vlaobj))
        
        (if (not (member layer layer_ja_adicionado))
          (progn
            (setvar 'clayer layer)
            
            (vla-addtext model_space layer (vlax-3d-point posicao_x_texto posicao_y_atual 0) 3)
            (command "_.rectang" (list posicao_x_inicial posicao_y_atual 0) "_area" 200 "_length" tamanho_das_linhas posicao_y_atual)       
            
            (setq posicao_y_atual (- posicao_y_atual espacamento_dos_retangulos))     
            (setq layer_ja_adicionado (cons layer layer_ja_adicionado))
          )
        )
      ) 
      
      (setq posicao_y_atual (- posicao_y_atual espacamento_de_categorias))
    )
    
  )
  
  (list (nth 0 ponto_de_insercao) posicao_y_atual (nth 2 ponto_de_insercao))
  
)

(defun legenda-hatch (tamanho_das_linhas
                        espacamento_dos_retangulos 
                        espacamento_de_categorias 
                        espacamento_de_textos 
                        ponto_de_insercao
                      /
                      posicao_x_inicial
                      posicao_x_texto
                      posicao_y_atual
                      hatch
                      quantidade
                      vlaobj
                      layer
                      layer_ja_adicionado
                      )
  
  (setq posicao_x_inicial (car ponto_de_insercao))
  (setq posicao_x_texto (+ (+ posicao_x_inicial tamanho_das_linhas) espacamento_de_textos))
  (setq posicao_y_atual (cadr ponto_de_insercao))

  (setq layer_ja_adicionado (list))
  
  (if (setq hatch (ssget "a" '(0 . "HATCH")))
    (progn
      (repeat (setq quantidade (sslength hatch))
        (setq vlaobj (vlax-ename->vla-object (ssname hatch (setq quantidade (1- quantidade)))))
        (setq layer (vla-get-layer vlaobj))
        
        (if (not (member layer layer_ja_adicionado))
          (progn
            
            (setvar 'clayer layer)
            (setq hatch_pattern (vla-get-patternname vlaobj))
            
            (vla-addtext model_space layer (vlax-3d-point posicao_x_texto posicao_y_atual 0) 3)
            (command "_.rectang" (list posicao_x_inicial posicao_y_atual 0) "_area" 200 "_length" tamanho_das_linhas posicao_y_atual)
            (command "_.hatch" hatch_pattern 0.1 0 "_last" "")   
            
            (setq posicao_y_atual (- posicao_y_atual espacamento_dos_retangulos))
            (setq layer_ja_adicionado (cons layer layer_ja_adicionado))
          )
        )
      )
      (setq posicao_y_atual (- posicao_y_atual espacamento_de_categorias))
    )
  )

  (list (nth 0 ponto_de_insercao) posicao_y_atual (nth 2 ponto_de_insercao))
)


(defun c:Legenda (/ *error* tamanho_das_linhas espacamento_de_textos espacamento_das_linhas espacamento_de_categorias ponto_de_insercao espacamento_de_categorias) 
  
  (defun *error* (msg)
    (or 
      (wcmatch (strcase msg t) "*break,*cancel*,*exit*") 
      (alert (strcat "ERROR: " msg "**"))
    )
    
    (vla-endundomark doc) 
  )
  
  (setq 
    acadObj (vlax-get-acad-object)
    doc (vla-get-activedocument acadObj)
    model_space (vla-get-modelspace doc)
  )
  
  (vla-startundomark doc)
  
  (command "_.style" "Standard" "" "0" "1" "0" "No" "No")
  
  
  (setvar 'cmdecho 0)
  (setvar 'osmode 0)
  
  ; Formatação
  (setq tamanho_das_linhas 20)
  (setq espacamento_das_linhas 10)
  (setq espacamento_dos_retangulos 18)
  (setq espacamento_de_categorias 10)
  (setq espacamento_de_textos 5)
  
  
  (setq ponto_de_insercao (getpoint "Selecione o ponto de insercao: "))
  
  
  (setq ponto_de_insercao 
    (legenda-linhas 
      tamanho_das_linhas 
      espacamento_das_linhas 
      espacamento_de_categorias 
      espacamento_de_textos
      ponto_de_insercao
    )
  )
  
  
  (setq ponto_de_insercao 
    (legenda-blocos
      tamanho_das_linhas
      espacamento_dos_retangulos
      espacamento_de_categorias
      espacamento_de_textos
      ponto_de_insercao
    )
  )
  
  (setq ponto_de_insercao
    (legenda-hatch
      tamanho_das_linhas
      espacamento_dos_retangulos
      espacamento_de_categorias
      espacamento_de_textos
      ponto_de_insercao
    )
  )

  (setvar 'osmode 4133) ; Reativa o osmode com os valores padroes
  (setvar 'cmdecho 1)
  
  (vla-endundomark doc) 
  
  (princ)
  
) ; defun Legendar
