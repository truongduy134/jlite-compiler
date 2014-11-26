OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc
OCAMLC = ocamlc

LEXER_NAME = jlite_lexer
PARSER_NAME = jlite_parser
MAIN_NAME = jlite_main
OUTPUT_DIR = $(shell pwd)/bin
$(shell mkdir -p $(OUTPUT_DIR))
CUR_DIR = $(shell pwd)

.PHONY : lexer parser main

all: main

main: helper parser lexer
	cp $(shell pwd)/$(MAIN_NAME).ml $(OUTPUT_DIR)/$(MAIN_NAME).ml
	$(OCAMLC) -I $(OUTPUT_DIR) -o $(OUTPUT_DIR)/$(MAIN_NAME) $(OUTPUT_DIR)/jlite_structs.cmo $(OUTPUT_DIR)/jlite_annotatedtyping.cmo $(OUTPUT_DIR)/ir3_structs.cmo $(OUTPUT_DIR)/jlite_toir3.cmo $(OUTPUT_DIR)/arm_structs.cmo $(OUTPUT_DIR)/common_helper.cmo $(OUTPUT_DIR)/ir3_to_cfg.cmo $(OUTPUT_DIR)/ir3_optimization.cmo $(OUTPUT_DIR)/ir3_toarm.cmo $(OUTPUT_DIR)/$(PARSER_NAME).cmo $(OUTPUT_DIR)/$(LEXER_NAME).cmo $(OUTPUT_DIR)/$(MAIN_NAME).ml
	cp $(OUTPUT_DIR)/$(MAIN_NAME) $(CUR_DIR)/$(MAIN_NAME)

lexer: parser
	$(OCAMLLEX) -o $(OUTPUT_DIR)/$(LEXER_NAME).ml $(LEXER_NAME).mll
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/$(LEXER_NAME).ml

parser: helper
	$(OCAMLYACC) -v -b $(OUTPUT_DIR)/$(PARSER_NAME) $(PARSER_NAME).mly
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/$(PARSER_NAME).mli
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/$(PARSER_NAME).ml

helper:
	cp $(shell pwd)/jlite_structs.ml $(OUTPUT_DIR)/jlite_structs.ml
	cp $(shell pwd)/ir3_structs.ml $(OUTPUT_DIR)/ir3_structs.ml
	cp $(shell pwd)/arm_structs.ml $(OUTPUT_DIR)/arm_structs.ml
	cp $(shell pwd)/jlite_annotatedtyping.ml $(OUTPUT_DIR)/jlite_annotatedtyping.ml
	cp $(shell pwd)/jlite_toir3.ml $(OUTPUT_DIR)/jlite_toir3.ml
	cp $(shell pwd)/ir3_toarm.ml $(OUTPUT_DIR)/ir3_toarm.ml
	cp $(shell pwd)/ir3_optimization.ml $(OUTPUT_DIR)/ir3_optimization.ml
	cp $(shell pwd)/ir3_to_cfg.ml $(OUTPUT_DIR)/ir3_to_cfg.ml
	cp $(shell pwd)/common_helper.ml $(OUTPUT_DIR)/common_helper.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c  $(OUTPUT_DIR)/jlite_structs.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/ir3_structs.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/arm_structs.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/jlite_toir3.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/jlite_annotatedtyping.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/common_helper.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/ir3_to_cfg.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/ir3_optimization.ml
	$(OCAMLC) -I $(OUTPUT_DIR) -c $(OUTPUT_DIR)/ir3_toarm.ml

clean:
	rm -rf $(OUTPUT_DIR)
	rm $(CUR_DIR)/$(MAIN_NAME)
