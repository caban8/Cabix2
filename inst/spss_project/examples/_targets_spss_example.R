library(targets)
library(tarchetypes)


# Set target options:
tar_option_set(
  packages = c(
    "tidyverse"
  )
)


tar_source("R/general/setup.R")
tar_source("R/fun")


cfg <- config::get()


list(
  # Setup
  tar_target(
    cache,
    memoise::cache_filesystem(cfg$paths$cache)
  ),

  # Set paths
  tar_target(
    dane_surowe_path,
    cfg$paths$dane_raw,
    format = "file"
  ),
  tar_target(
    metoda_path,
    cfg$paths$metoda,
    format = "file"
  ),

  # Import data
  tar_target(
    metoda_imported,
    Cabix2::import_metoda(metoda_path)
  ),
  tar_target(
    metoda,
    Cabix2::join_analyses_hypotheses(metoda_imported)
  ),
  tar_target(
    dane_surowe,
    readxl::read_excel(dane_surowe_path)
  ),

  # Processing data
  tar_target(
    dane_renamed,
    etl_rename(dane_surowe)
  ),
  tar_target(
    vars_labels,
    set_vars_labs(dane_surowe, dane_renamed)
  ),
  tar_target(
    dane_modified,
    etl_modify_vars(dane_renamed)
  ),
  tar_target(
    dane_engineered,
    etl_engineer(dane_modified)
  ),
  tar_target(
    dane_labelled,
    assign_labs(dane_engineered, vars_labels)
  ),
  tar_target(
    spss_exported_path,
    export_spss(dane_labelled, cfg$paths$dane_spss),
    format = "file"
  ),
  tar_target(
    dane_spss,
    haven::read_sav(spss_exported_path)
  ),

  # Reliabilities
  tar_target(
    reliabilities,
    calc_reliability(dane_engineered)
  ),
  # Report blueprint
  tar_target(
    report_blueprint,
    Cabix2::create_blueprint(metoda)
  ),

  # Analizy
  tar_target(
    an_desc,
    Cabix2::analyze_des_tab(
      dane_spss,
      OSI_SzP_skala1,
      OSI_SzP_skala2,
      OSI_SzP_skala3,
      OSI_SzP_skala4,
      OSI_zdrowieA_skala1,
      OSI_zdrowieB_skala1,
      OSI_ZP_skala1,
      OSI_ZP_skala2,
      OSI_ZP_skala3,
      OSI_ZP_skala4,
      OSI_ZP_skala5,
      OSI_ZP_skala6,
      OSI_ZszS_skala1,
      OSI_ZszS_skala2,
      OSI_ZszS_skala3,
      OSI_ZszS_skala4,
      OSI_ZszS_skala5,
      OSI_ZszS_skala6,
      cache = cache,
      model = cfg$ai$model,
      cfg_index = cfg$descriptives
    )
  ),
  tar_target(
    an_demographics,
    Cabix2::analyze_demographics(
      dane_spss,
      categorical_vars = c(
        plec,
        wyksztalcenie,
        stan_cywilny,
        miejsce_zamieszkania
      ),
      continuous_vars = c(
        wiek
      ),
      cache = cache
    )
  ),
  tar_target(
    an_cor,
    Cabix2::analyze_cor(
      report_blueprint,
      dane_spss,
      cache = cache,
      model = cfg$ai$model,
      analysis_type = "spearman",
      cor_args = list(method = "spearman"),
      flextable_args = list(one.cor = TRUE)
    )
  ),
  tar_target(
    an_umann,
    Cabix2::analyze_comparison_bg1(
      report_blueprint,
      dane_spss,
      cache = cache,
      model = cfg$ai$model,
      analysis_type = "umann",
      test_args = list(test = "u_mann")
    )
  ),

  # Report
  tar_target(
    report_arranged,
    Cabix2::combine_and_rearrange_reports(
      an_desc,
      an_demographics,
      an_cor,
      an_umann
    )
  ),
  tar_target(
    report_with_hypotheses,
    AIinterpreter::nar_hypotheses(
      report_arranged,
      hipoteza = hipoteza,
      analysis_type = typ_long,
      tab_nr = tab_nr,
      cache = cache,
      output = wprowadzenie
    )
  ),
  tar_target(
    report_frame,
    AIinterpreter::nar_frame(
      data = report_with_hypotheses,
      cache = cache,
      reliability_table = reliabilities,
      vars = c(wprowadzenie, interpretacja),
      model = "gpt-5.4",
      cfg = cfg$report$introduction,
      language = "pl"
    )
  ),
  tar_target(
    report_final,
    bind_rows(
      report_frame,
      report_with_hypotheses
    )
  ),

  tar_target(
    report_interpretacje_exported,
    Cabix2::export_interpretacje(
      report_final,
      cfg$paths$interpretacje,
      vars = c(tab_nr, section, caption, wprowadzenie, interpretacja)
    )
  ),
  tar_target(
    interpretacje_edited_path,
    cfg$paths$interpretacje_edited,
    format = "file"
  ),
  tar_target(
    interpretacje_edited_imported,
    yaml::read_yaml(interpretacje_edited_path)
  ),
  tar_target(
    report_final_rejoined,
    Cabix2::rejoin_interpretacje(
      report_final,
      interpretacje_edited_imported,
      vars = c(tab_nr, flextable, analiza, section)
    )
  ),
  tar_target(
    report_evaluation,
    AIinterpreter::nar_evaluate(
      report_final_rejoined,
      wprowadzenie,
      analiza,
      interpretacja,
      caption,
      tab_nr,
      cache = cache,
      model = "gpt-5.4"
    )
  ),

  # Markdown
  tar_target(
    markdown_doc,
    Cabix2::create_rmd_report(
      report_final_rejoined,
      section_col = section,
      target_objs = report_final_rejoined,
      wprowadzenie,
      flextable,
      caption,
      interpretacja
    )
  ),
  tar_target(
    markdown_written_path,
    Cabix2::write_markdown(markdown_doc, cfg$paths$markdown),
    format = "file"
  ),
  tar_target(
    markdown_report,
    {
      invisible(markdown_doc)
      rmarkdown::render(
        input = markdown_written_path,
        output_file = cfg$paths$markdown_out,
        quiet = TRUE
      )
    },
    format = "file"
  ),

  # Export tabeli
  tar_target(
    flextables_docx,
    Cabflex2::flex_export(report_final, tab_nr, caption, flextable)
  ),
  tar_target(
    flextables_png,
    {
      invisible(NULL)
      Cabflex2::flex_export(
        report_final,
        tab_nr,
        caption,
        flextable,
        extension = "png"
      )
    }
  ),

  tar_target(
    markdown_report_evaluation,
    Cabix2::markdown_simple_html(
      report_evaluation,
      title = " - Wiktoria Pietruszka",
      heading = "Evaluation"
    )
  ),
  tar_target(
    markdown_evaluation_html_path,
    Cabix2::write_markdown(markdown_report_evaluation, "report_evaluation.Rmd"),
    format = "file"
  ),
  tar_target(
    markdown_evaluation_rendered,
    rmarkdown::render(
      input = markdown_evaluation_html_path,
      quiet = TRUE
    ),
    format = "file"
  )
)
