# app.py (versão final com todas as correções)

# --- 1. Importação das Bibliotecas Necessárias ---
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import LogLocator, FuncFormatter
from shiny import App, ui, render
import numpy as np

# --- 2. Carregamento e Processamento dos Dados ---
try:
    # Carrega os dados brutos
    dim_teachers = pd.read_csv("dim_teachers.csv")
    fact_entries = pd.read_csv("fct_teachers_entries.csv")
    fact_interactions = pd.read_csv("fct_teachers_contents_interactions.csv")

    # **** CORREÇÃO 1: Limpeza de Colunas Melhorada ****
    # Remove espaços, converte para minúsculas E SUBSTITUI ESPAÇOS POR UNDERSCORE (_).
    dim_teachers.columns = dim_teachers.columns.str.strip().str.lower().str.replace(' ', '_')
    fact_entries.columns = fact_entries.columns.str.strip().str.lower().str.replace(' ', '_')
    fact_interactions.columns = fact_interactions.columns.str.strip().str.lower().str.replace(' ', '_')
    # *************************************************

    # --- Cálculo da Frequência ---
    df_frequencia = fact_entries.groupby(['teacher_id', 'ano']).agg(
        frequencia=('data', 'count')
    ).reset_index()

    # --- Cálculo do Total de Interações ---
    df_interacoes = fact_interactions.groupby(['teacher_id', 'ano']).agg(
        total_de_interacoes=('id', 'count')
    ).reset_index()

    # --- Junção (Merge) das Métricas ---
    df_final = pd.merge(
        df_frequencia,
        df_interacoes,
        on=['teacher_id', 'ano'],
        how='outer'
    )
    df_final.fillna(0, inplace=True)
    
    numeric_cols = df_final.select_dtypes(include=['number']).columns.tolist()
    cols_to_remove = ['teacher_id', 'ano']
    col_choices = [col for col in numeric_cols if col not in cols_to_remove]

except FileNotFoundError as e:
    print(f"Erro: Arquivo não encontrado - {e.filename}. Verifique se o app.py e os CSVs estão na mesma pasta.")
    df_final = pd.DataFrame()
    col_choices = []
except Exception as e:
    print(f"Ocorreu um erro durante o processamento dos dados: {e}")
    df_final = pd.DataFrame()
    col_choices = []


# --- 3. Definição da Interface do Utilizador (UI) ---
# **** CORREÇÃO 2: Removido o 'ui.main()' ****
app_ui = ui.page_fluid(
    ui.h2("Dashboard Interativo: Análise de Uso da Plataforma"),
    ui.layout_sidebar(
        ui.sidebar(
            ui.h4("Controlos de Visualização"),
            ui.input_select(
                "ano",
                "Selecione o Ano:",
                choices=[int(x) for x in sorted(df_final["ano"].unique())] if not df_final.empty and "ano" in df_final.columns else []
            ),
            ui.input_select(
                "xcol",
                "Selecione a Variável X (Envolvimento):",
                choices=col_choices,
                selected="total_de_interacoes"
            ),
            ui.input_select(
                "ycol",
                "Selecione a Variável Y (Frequência):",
                choices=col_choices,
                selected="frequencia"
            ),
        ),
        # O gráfico vai diretamente como o segundo argumento do layout_sidebar
        ui.output_plot("scatter_plot")
    ),
)
# *************************************************


# --- 4. Lógica do Servidor (Server) ---
def server(input, output, session):
    @output
    @render.plot
    def scatter_plot():
        if df_final.empty or not input.ano():
            return None 

        ano_selecionado = int(input.ano())
        xcol = input.xcol()
        ycol = input.ycol()
        
        df = (
            df_final[[xcol, ycol, "ano"]]
            .query(f"ano == {ano_selecionado}")
            .copy()
        )
        df.dropna(inplace=True)

        df_filtrado = df[(df[xcol] > 0) & (df[xcol] <= 1000) & (df[ycol] > 0) & (df[ycol] <= 400)]

        fig, ax = plt.subplots(figsize=(10, 7))
        ax.set_title(f"Dados para o Ano de {ano_selecionado}", fontsize=12, fontweight="bold")
        fig.suptitle(f"{ycol.replace('_', ' ').title()} vs. {xcol.replace('_', ' ').title()}", fontsize=16)
        ax.set_ylabel(f"{ycol.replace('_', ' ').title()}")
        
        if not df_filtrado.empty:
            ax.scatter(df_filtrado[xcol], df_filtrado[ycol], s=14, alpha=0.7, edgecolors="none", rasterized=True, color="#3b2087")
            ax.set_xscale("log")
            ax.set_xlim(1, 1000)
            ax.set_ylim(0, 400)
            ax.grid(True, which="both", linestyle="--", alpha=0.3)
            ax.set_xlabel(f"{xcol.replace('_', ' ').title()} (escala log)")
            ax.xaxis.set_major_locator(LogLocator(base=10))
            ax.xaxis.set_minor_locator(LogLocator(base=10, subs=(2, 5)))
            ax.xaxis.set_major_formatter(FuncFormatter(lambda v, p: f"{int(v)}"))
        else:
            ax.text(0.5, 0.5, "Não há dados para a seleção atual.", horizontalalignment='center', verticalalignment='center', transform=ax.transAxes, fontsize=14, color='red')
            ax.set_xlim(0, 1)
            ax.set_ylim(0, 1)
            ax.set_xlabel(f"{xcol.replace('_', ' ').title()}")
        
        plt.tight_layout(rect=[0, 0.03, 1, 0.95])
        return fig

# --- 5. Cria e Inicia a Aplicação ---
app = App(app_ui, server)