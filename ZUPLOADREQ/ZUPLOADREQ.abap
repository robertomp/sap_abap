*&---------------------------------------------------------------------*
*& Report  ZUPLOADREQ
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZUPLOADREQ.

DATA: vl_cofile            TYPE string,
      vl_data              TYPE string,
      vl_osys              TYPE string,
      vl_path_cofile       TYPE sapb-sappfad,
      vl_path_data         TYPE sapb-sappfad,
      vl_targetpath_cofile TYPE sapb-sappfad,
      vl_targetpath_data   TYPE sapb-sappfad.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS: p_req   TYPE e070-trkorr,
            p_local TYPE rlgrap-filename.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb_up RADIOBUTTON GROUP rb.
SELECTION-SCREEN COMMENT 10(10) FOR FIELD rb_up.
PARAMETERS rb_down RADIOBUTTON GROUP rb.
SELECTION-SCREEN COMMENT 30(10) FOR FIELD rb_down.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b2.

*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_local.
*--------------------------------------------------------------------*
  PERFORM f4_dir CHANGING p_local.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*
  MOVE p_req(3) TO vl_osys.

  CONCATENATE 'K' p_req+4(6) '.' vl_osys INTO vl_cofile.
  CONCATENATE 'R' p_req+4(6) '.' vl_osys INTO vl_data.

  PERFORM ger_dir_tran  USING     vl_cofile vl_data vl_targetpath_cofile vl_targetpath_data.

  PERFORM ger_dir_local CHANGING  vl_path_cofile vl_path_data.

*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*

  CASE abap_true.

    WHEN rb_up.
      CALL FUNCTION 'ARCHIVFILE_CLIENT_TO_SERVER'
        EXPORTING
          path             = vl_path_cofile
          targetpath       = vl_targetpath_cofile
        EXCEPTIONS
          error_file       = 1
          no_authorization = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'ARCHIVFILE_CLIENT_TO_SERVER'
        EXPORTING
          path             = vl_path_data
          targetpath       = vl_targetpath_data
        EXCEPTIONS
          error_file       = 1
          no_authorization = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.

    WHEN rb_down.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
        EXPORTING
          path             = vl_targetpath_data
          targetpath       = vl_path_data
        EXCEPTIONS
          error_file       = 1
          no_authorization = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
        EXPORTING
          path             = vl_targetpath_cofile
          targetpath       = vl_path_cofile
        EXCEPTIONS
          error_file       = 1
          no_authorization = 2
          OTHERS           = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

  ENDCASE.  " CASE abap_true.

**********************************************************************
**********************************************************************
**********************************************************************

FORM ger_dir_tran USING    pi_fil_k      TYPE string
                           pi_fil_r      TYPE string
                  CHANGING pv_fil_s_k    TYPE sapb-sappfad
                           pv_fil_s_r    TYPE sapb-sappfad.
  DATA:
    vl_dir    TYPE dirname_al11,
    vl_file_k TYPE dirname_al11,
    vl_file_r TYPE dirname_al11,
    vl_sep    TYPE c LENGTH 1.

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TRANS'
                     ID 'VALUE' FIELD vl_dir.

  FIND '\' IN vl_dir.
  IF sy-subrc IS INITIAL.
    MOVE '\' TO vl_sep.
  ELSE.
    MOVE '/' TO vl_sep.
  ENDIF.

  CONCATENATE vl_dir 'cofiles' pi_fil_k INTO pv_fil_s_k SEPARATED BY vl_sep.
  CONCATENATE vl_dir 'data'    pi_fil_r INTO pv_fil_s_r SEPARATED BY vl_sep.
ENDFORM.                    " GER_DIR_TRAN

FORM ger_dir_local  CHANGING pv_fil_s_k    TYPE sapb-sappfad
                             pv_fil_s_r    TYPE sapb-sappfad.
  DATA:
    vl_sep    TYPE c LENGTH 1.

  FIND '\' IN p_local.
  IF sy-subrc IS INITIAL.
    MOVE '\' TO vl_sep.
  ELSE.
    MOVE '/' TO vl_sep.
  ENDIF.

  CONCATENATE p_local vl_cofile INTO pv_fil_s_k SEPARATED BY vl_sep.
  CONCATENATE p_local vl_data   INTO pv_fil_s_r SEPARATED BY vl_sep.

ENDFORM.                    " GER_DIR_TRAN

*&---------------------------------------------------------------------*
*&      Form  F4_DIR
*&---------------------------------------------------------------------*
FORM f4_dir  CHANGING pv_dir TYPE rlgrap-filename .
  DATA:
    vl_ini TYPE string,
    vl_dir TYPE string.


  vl_ini = pv_dir.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder       = vl_ini
    CHANGING
      selected_folder      = vl_dir
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF vl_dir <> space.
    pv_dir = vl_dir.
  ENDIF.
ENDFORM.                                                    " F4_DIR
