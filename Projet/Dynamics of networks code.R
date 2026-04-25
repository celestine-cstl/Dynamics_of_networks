
# Dynamics of networks 
# Evaluating the Impact of a Research Funding Policy on Individual Outcomes and Collaborations
# ATALLAH Joanne & COUSTILLAC Cêlestine

###############################################################################.
## 0 - Data preparation ----
###############################################################################.

# Cêlestine's part 
### 0.1 - Cleaning and harmonizing the dyadic panel ############################
# Load required packages
library(data.table)
library(ggplot2)
library(scales)
library(igraph)
library(fixest)

# Import the dataset
df <- fread("data/panel_network_unbalanced_all_group_3.csv", sep = ";")

# Drop all columns whose names start with "feature" (not used in the analysis)
df[, (grep("^feature", names(df), value = TRUE)) := NULL]

# Missingness snapshot (top 5)
miss <- df[, lapply(.SD, \(x) mean(is.na(x))), .SDcols = names(df)]
miss_long <- melt(miss, measure.vars = names(miss), variable.name = "var", value.name = "missing_rate")
setorder(miss_long, -missing_rate)
print(head(miss_long, 5))

# id_dyad : order-invariant dyad identifier: (i,j) and (j,i) get the same id
df[, id_dyad := paste(pmin(id_1, id_2), pmax(id_1, id_2), sep = "_")]

# agemean : mean age within the dyad
df[, agemean := (age_1 + age_2) / 2]

# same_group : 1 if the dyad members are in the same group on that row (intra-group), 0 otherwise (extra-group)
df[, same_group := as.integer(goup_id_1 == goup_id_2)]

# Rename variables to improve readability and match the definitions in the project guidelines.
setnames(df, 
         old = c("collab","link", 
                 "goup_id_1","goup_id_2",
                 "type_1_1", "type_2_1", "type_1_2", "type_2_2"),
         new = c("yearly_collabs_dyad","link_dyad", 
                 "group_id_1","group_id_2",
                 "core_1", "female_1", "core_2", "female_2"))


# Define the target column order to standardize the dataset structure.
final_order <- c("year","post","id_1","id_2","id_dyad","id_dyad_in_group",
                 "group_id_1","group_id_2","group_treated_1","group_treated_2", "same_group",
                 "yearly_collabs_dyad","link_dyad",
                 "core_1","core_2","female_1","female_2",
                 "age_1","age_2","agemax","agedif","agemean",
                 "outcome_1_1","outcome_1_2","outcome_2_1","outcome_2_2","outcome_3_1","outcome_3_2",
                 "selection_1","selection_2",
                 "field_1_1","field_2_1","field_3_1","field_4_1","field_5_1","field_6_1","field_7_1","field_8_1",
                 "field_1_2","field_2_2","field_3_2","field_4_2","field_5_2","field_6_2","field_7_2","field_8_2"
)
df <- df[, c(intersect(final_order, names(df)), setdiff(names(df), final_order)), with = FALSE]



### 0.2 - Individual-level panel : individual x group x year ###################
# The policy treatment is defined at the group level, but outcomes can be studied 
# at the individual level. The dyadic dataset (df) is great to study links i-j, 
# but for diff-in-dff/event-study on individual outcomes, we want one row per 
# individual i in group g and year t.

# Convert the dyadic data into a long format where each row is i, partner j, group g of i, year t
dt_long <- rbindlist(list(
  # Member 1
  df[, .(year, post,
         group_id = group_id_1,
         id = id_1,
         partner = id_2,
         partner_group_id = group_id_2,
         same_group = as.integer(group_id_1 == group_id_2),
         yearly_collabs_dyad, link_dyad,
         group_treated = group_treated_1,
         partner_treated = group_treated_2,
         selection = selection_1,
         female = female_1,
         core = core_1,
         age = age_1,
         outcome_1 = outcome_1_1,
         outcome_2 = outcome_2_1,
         outcome_3 = outcome_3_1,
         field_1 = field_1_1,
         field_2 = field_2_1, 
         field_3 = field_3_1,
         field_4 = field_4_1,
         field_5 = field_5_1,
         field_6 = field_6_1,
         field_7 = field_7_1,
         field_8 = field_8_1
  )],
  # Member 2
  df[, .(year, post,
         group_id = group_id_2,
         id = id_2,
         partner = id_1,
         partner_group_id = group_id_1,
         same_group = as.integer(group_id_2 == group_id_1),
         yearly_collabs_dyad, link_dyad,
         group_treated = group_treated_2,
         partner_treated = group_treated_1,
         selection = selection_2,
         female = female_2,
         core = core_2,
         age = age_2,
         outcome_1 = outcome_1_2,
         outcome_2 = outcome_2_2,
         outcome_3 = outcome_3_2,
         field_1 = field_1_2,
         field_2 = field_2_2, 
         field_3 = field_3_2,
         field_4 = field_4_2,
         field_5 = field_5_2,
         field_6 = field_6_2,
         field_7 = field_7_2,
         field_8 = field_8_2
  )]
), use.names = TRUE, fill = TRUE)

# Take the first non-missing value
first_non_na <- function(x) {
  i <- which(!is.na(x))[1]
  if (is.na(i)) x[1] else x[i]
}

# Deduplicate at the id, group, year, partner level
dt_long_partner <- dt_long[, .(
  yearly_collabs_dyad = max(yearly_collabs_dyad, na.rm = TRUE),
  link_dyad = max(link_dyad, na.rm = TRUE),
  same_group = max(same_group, na.rm = TRUE),
  post = first_non_na(post),
  group_treated = first_non_na(group_treated),
  partner_treated = first_non_na(partner_treated),
  selection = first_non_na(selection),
  female = first_non_na(female),
  core = first_non_na(core),
  age = first_non_na(age),
  outcome_1 = first_non_na(outcome_1),
  outcome_2 = first_non_na(outcome_2),
  outcome_3 = first_non_na(outcome_3),
  field_1 = first_non_na(field_1),
  field_2 = first_non_na(field_2),
  field_3 = first_non_na(field_3),
  field_4 = first_non_na(field_4),
  field_5 = first_non_na(field_5),
  field_6 = first_non_na(field_6),
  field_7 = first_non_na(field_7),
  field_8 = first_non_na(field_8)
), by = .(year, group_id, id, partner)]

# Aggregate to the "individual × group × year" level (one row per i-g-t).
df_ind <- dt_long_partner[, .(
  # Outcome: at least one collaboration in that year for ego i in group g
  link_ind = as.integer(any(link_dyad == 1)),
  nb_partners = uniqueN(partner[link_dyad == 1]), # Distinct active partners
  total_collab = sum(yearly_collabs_dyad, na.rm = TRUE), # Total collaboration volume
  
  # Internal vs external distinct partners
  n_internal_partners = uniqueN(partner[link_dyad == 1 & same_group == 1]),
  n_external_partners = uniqueN(partner[link_dyad == 1 & same_group == 0]),
  
  # Internal vs external collaboration volumes
  internal_collab = sum(yearly_collabs_dyad[same_group == 1], na.rm = TRUE),
  external_collab = sum(yearly_collabs_dyad[same_group == 0], na.rm = TRUE),
  
  post = first_non_na(post),
  group_treated = first_non_na(group_treated),
  selection = first_non_na(selection),
  female = first_non_na(female),
  core = first_non_na(core),
  age = first_non_na(age),
  outcome_1 = first_non_na(outcome_1),
  outcome_2 = first_non_na(outcome_2),
  outcome_3 = first_non_na(outcome_3),
  field_1 = first_non_na(field_1),
  field_2 = first_non_na(field_2),
  field_3 = first_non_na(field_3),
  field_4 = first_non_na(field_4),
  field_5 = first_non_na(field_5),
  field_6 = first_non_na(field_6),
  field_7 = first_non_na(field_7),
  field_8 = first_non_na(field_8)
), by = .(year, group_id, id)]

# Define analysis samples :
df_sel <- df_ind[selection == 1] # restricted sample (selection criterion)

# Ensure consistent variable types before estimation
df_sel[, `:=`(
  year = as.integer(year),
  post = as.integer(post),
  group_treated = as.integer(group_treated),
  selection = as.integer(selection),
  female = as.integer(female),
  core = as.integer(core),
  age = as.numeric(age),
  id = as.character(id),
  group_id = as.character(group_id),
  outcome_1 = as.numeric(outcome_1),
  outcome_2 = as.numeric(outcome_2),
  outcome_3 = as.numeric(outcome_3),
  field_1 = as.integer(field_1),
  field_2 = as.integer(field_2),
  field_3 = as.integer(field_3),
  field_4 = as.integer(field_4),
  field_5 = as.integer(field_5),
  field_6 = as.integer(field_6),
  field_7 = as.integer(field_7),
  field_8 = as.integer(field_8),
  link_ind = as.integer(link_ind),
  nb_partners = as.integer(nb_partners),
  total_collab = as.numeric(total_collab),
  n_internal_partners = as.integer(n_internal_partners),
  n_external_partners = as.integer(n_external_partners),
  internal_collab = as.numeric(internal_collab),
  external_collab = as.numeric(external_collab)
)]







###############################################################################.
## I - Descriptive analysis ----
###############################################################################.

# Time period
cat("Min year =", min(df$year, na.rm = TRUE), "\n")
cat("Max year =", max(df$year, na.rm = TRUE), "\n")

# Number of unique individuals
individuals <- unique(na.omit(c(df$id_1, df$id_2)))
cat("Number of unique individuals =", length(individuals), "\n")

# Number of unique groups
groups_unique <- unique(na.omit(c(df$group_id_1, df$group_id_2)))
cat("Number of unique groups =", length(groups_unique), "\n")

# Number of treated/non-treated groups
g1 <- df[, .(group_id = group_id_1, group_treated = group_treated_1)]
g2 <- df[, .(group_id = group_id_2, group_treated = group_treated_2)]

groups_dt <- unique(na.omit(rbindlist(list(g1, g2), use.names = TRUE)))

treated_by_group <- groups_dt[, .(group_treated = max(group_treated, na.rm = TRUE)), by = group_id]

n_treated     <- treated_by_group[group_treated == 1, .N]
n_non_treated <- treated_by_group[group_treated == 0, .N]

cat("Number of treated groups =", n_treated, "\n")
cat("Number of non-treated groups =", n_non_treated, "\n")

# Our network is analyzed over the 2005–2019 period: it includes a total of 9,031 
# individuals and 255 groups. For the overall description, we distinguish two 
# sub-periods corresponding to the implementation of the policy : before 2012 and 
# from 2012 onward. Among these groups, 128 are treated and 127 are not treated, 
# which corresponds to a balanced split.


# For each (year, post, id), we count how many distinct groups the individual belongs to
# within that year
plot_dt <- df_ind[, .(n_groups = uniqueN(group_id)), 
                  by = .(year, post, id)][
                    , .N, by = .(post, n_groups)
                    # Convert counts into percentages *within each period*.
                  ][, pct := 100 * N / sum(N), by = post
                  ][, period := factor(post, levels = c(0, 1),
                                       labels = c("Pre-treatment", "Post-treatment"))
                  ][, n_groups_f := factor(n_groups, levels = sort(unique(n_groups)))
                  ][# Format labels with more decimals for very small percentages
                    , label := fifelse(pct == 0, NA_character_,
                                       fifelse(pct < 0.1, sprintf("%.3f%%", pct),
                                               fifelse(pct < 1, sprintf("%.2f%%", pct),
                                                       sprintf("%.1f%%", pct))))
                  ]
# Raise labels above bars
plot_dt[, label_y := pct + fifelse(n_groups %in% 1:3, 3.0,  
                                   fifelse(pct < 0.1, 3.5,
                                           fifelse(pct < 1, 2.8, 
                                                   fifelse(pct < 5, 2.0,   
                                                           fifelse(pct < 20, 1.6,
                                                                   1.4)))))]
# Side-by-side bars compare the distribution of n_groups between pre and post periods.
ggplot(plot_dt, aes(x = n_groups_f, y = pct, fill = period)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(y = label_y, label = label),
            position = position_dodge(width = 0.8),
            angle = 90,
            vjust = 0,
            size = 3,
            na.rm = TRUE) +
  scale_fill_manual(values = c("Pre-treatment" = "#BFC5C7",
                               "Post-treatment" = "#5A8FA0")) +
  coord_cartesian(ylim = c(0, max(plot_dt$label_y, na.rm = TRUE) + 2), clip = "off") +
  labs(title = "Number of groups per individual within the same year (Pre vs Post)",
       x = "Number of groups",
       y = "% of individuals",
       fill = NULL) +
  theme_classic()
# Most individuals are affiliated with a single group within a given year (54.9% 
# pre-treatment vs 53.8% post-treatment). The distribution is very similar across 
# periods : the share with two groups slightly increases after the policy (25.1% 
# to 26.0%), while the share with three groups remains almost unchanged (13.5% 
# to 13.3%). Membership in four or more groups is rare in both periods.


# Do people change group over the years?  (i.e., group_id not invariant for an id)
movers <- df_ind[, .(n_groups = uniqueN(group_id)), by = id][n_groups > 1, .N]
total_ids <- uniqueN(df_ind$id)
cat("Do individuals change group over time? ",
    ifelse(movers > 0, "TRUE", "FALSE"), "\n") # TRUE 

# Can individuals be observed in both treated and control groups ?
both_status <- df_ind[!is.na(group_treated),
                      .(ever_treated = any(group_treated==1),
                        ever_control = any(group_treated==0)),
                      by = id][ever_treated & ever_control, .N]
cat("Can individuals be in a treated and a non-treated group over the period ? ",
    ifelse(both_status > 0, "TRUE", "FALSE"), "\n") # TRUE 


# Pre vs Post network metrics 
df_net_source <- df

# Collapse potential duplicates at the dyad-year level (safe with your id_dyad)
edges_dyad_year <- df_net_source[link_dyad == 1, .(
  from = as.character(pmin(id_1, id_2)),
  to = as.character(pmax(id_1, id_2)),
  weight = sum(yearly_collabs_dyad, na.rm = TRUE),
  post = max(post, na.rm = TRUE)
), keyby = .(id_dyad, year)]

all_ids_net <- unique(na.omit(c(df_net_source$id_1, df_net_source$id_2)))
vertices_net <- data.frame(name = as.character(all_ids_net))

build_graph_period <- function(ed) {
  g <- graph_from_data_frame(d = ed[, .(from, to, weight)],
                             directed = FALSE,
                             vertices = vertices_net)
  g <- simplify(g,
                remove.loops = TRUE,
                remove.multiple = TRUE,
                edge.attr.comb = list(weight = "sum"))
  g
}
g_pre_net  <- build_graph_period(edges_dyad_year[post == 0])
g_post_net <- build_graph_period(edges_dyad_year[post == 1])

# Global metrics
global_metrics <- function(g) {
  data.table(n_nodes = vcount(g),
             n_edges = ecount(g),
             density = edge_density(g, loops = FALSE),
             clustering_global = transitivity(g, type = "global"),
             clustering_avg_local = transitivity(g, type = "average"))
}
net_summary_pre  <- cbind(data.table(period = "Pre-treatment"),  global_metrics(g_pre_net))
net_summary_post <- cbind(data.table(period = "Post-treatment"), global_metrics(g_post_net))
net_summary <- rbindlist(list(net_summary_pre, net_summary_post))

net_summary




# We now describe the profile of individuals in the network along three dimensions : 
# gender, core/non-core status, and age (measured in 2005).
ind1 <- df[, .(id = id_1, core = core_1, female = female_1)]
ind2 <- df[, .(id = id_2, core = core_2, female = female_2)]

ind_all <- rbindlist(list(ind1, ind2), use.names = TRUE)
ind_all <- ind_all[!is.na(id)]
base_ind <- ind_all[, .(
  core = if (all(is.na(core))) NA_integer_ else as.integer(names(which.max(table(core)))),
  female = if (all(is.na(female))) NA_integer_ else as.integer(names(which.max(table(female))))
), by = id]

# % Male/Female
female_pct <- base_ind[!is.na(female), .N, by = female][, pct := 100 * N / sum(N)]

female_plot <- merge(
  data.table(female = c(0, 1), label = c("Male", "Female")),
  female_pct, by = "female", all.x = TRUE
)[, pct := fifelse(is.na(pct), 0, pct)]

# sort from highest to lowest
female_plot[, label := factor(label, levels = label[order(-pct)])]

ggplot(female_plot, aes(x = label, y = pct, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = c("Male" = "#5A8FA0", "Female" = "#BFC5C7")) +
  labs(title = "Gender composition of the network", x = NULL, y = "%") +
  theme_classic()


# % Core/Non-core
core_pct <- base_ind[!is.na(core), .N, by = core][, pct := 100 * N / sum(N)]

core_plot <- merge(
  data.table(core = c(0, 1), label = c("Non-core", "Core")),
  core_pct, by = "core", all.x = TRUE
)[, pct := fifelse(is.na(pct), 0, pct)]

# sort from highest to lowest
core_plot[, label := factor(label, levels = label[order(-pct)])]

ggplot(core_plot, aes(x = label, y = pct, fill = label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = c("Non-core" = "#5A8FA0", "Core" = "#BFC5C7")) +
  labs(title = "Core vs Non-core composition of the network", x = NULL, y = "%") +
  theme_classic()

# The network includes 57.7% men and 42.3% women, and it is mainly composed of 
# non-core individuals (76%), compared to 24% core individuals.


# Age distribution in 2005
age1 <- df[, .(id = id_1, age_obs = age_1, year_obs = year)]
age2 <- df[, .(id = id_2, age_obs = age_2, year_obs = year)]

ind_all_age <- rbindlist(list(age1, age2), use.names = TRUE, fill = TRUE)
ind_all_age <- ind_all_age[!is.na(id) & !is.na(age_obs) & !is.na(year_obs)]

# 1 row per id: keep the earliest year_obs
base_age <- ind_all_age[, .SD[which.min(year_obs)], by = id]
base_age[, age_2005 := age_obs - (year_obs - 2005)]
base_age <- base_age[!is.na(age_2005)]
base_age[, age_2005 := as.integer(round(age_2005))]

age_dist <- base_age[, .N, by = age_2005][order(age_2005)][, pct := 100 * N / sum(N)]

xmin <- min(age_dist$age_2005, na.rm = TRUE)
xmax <- max(age_dist$age_2005, na.rm = TRUE)
breaks_5 <- seq(from = ceiling(xmin / 5) * 5, to = floor(xmax / 5) * 5, by = 5)

ggplot(age_dist, aes(x = age_2005, y = pct)) +
  geom_col(width = 0.8, fill = "#5A8FA0") +
  scale_x_continuous(breaks = breaks_5) +
  labs(title = "Age distribution in 2005", x = "Age", y = "% of individuals") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))
# The age distribution in 2005 is mainly concentrated between approximately 25 
# and 45 years old, with a peak around the thirties/forties, and relatively few 
# very young or very old individuals.

# These elements indicate a rather male-dominated network, largely non-core, 
# and composed of a population with a mostly intermediate age.


df_ind[selection==1 & year<=2011, 
       summary(field_1 + field_2 + field_3 + field_4 + field_5 + field_6 + field_7 + field_8)]
# Field indicators are not mutually exclusive: individuals can belong to multiple scientific fields.
# We use the full sample and all years. Since fields are time-invariant in the data,
# we collapse to one row per individual and set field_k = 1 if it is ever observed as 1.

fields <- paste0("field_", 1:8)
df_id <- df_ind[
  , lapply(.SD, function(x) as.integer(max(x == 1, na.rm = TRUE))),
  by = id, .SDcols = fields
]

# Share of individuals by field
share_dt <- data.table(field = fields,
                       pct = 100 * sapply(fields, function(f) mean(df_id[[f]] == 1, na.rm = TRUE)))

ggplot(share_dt, aes(x = field, y = pct)) +
  geom_col(fill = "#5A8FA0", width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Share of individuals by field (full sample, all years)",
       x = "", y = "% of individuals") +
  theme_classic()
# The distribution is highly concentrated: field_3 covers 100% of individuals, 
# and field_1 and field_2 include most researchers (about 83% and 81%). Fields 
# 4–8 are rare (around 2–6% each).



# As the Diff-in-Diff strategy relies on the selected sample (selection = 1), we will conduct 
# the impact analysis using the corresponding individual-level panel df_sel. 

# Do individuals change group over time (within selection==1)?
movers_sel <- df_sel[, .(n_groups = uniqueN(group_id)), by = id][n_groups > 1, .N]
total_ids_sel <- uniqueN(df_sel$id)
cat("Selection==1:\n")
cat("Do individuals change group over time? ",
    ifelse(movers_sel > 0, "TRUE", "FALSE"), "\n")

# Can individuals be observed in both treated and control groups (within selection==1)?
both_status_sel <- df_sel[!is.na(group_treated),
                          .(ever_treated = any(group_treated == 1),
                            ever_control = any(group_treated == 0)),
                          by = id][ever_treated & ever_control, .N]
cat("Can individuals be in a treated and a non-treated group over the period? ",
    ifelse(both_status_sel > 0, "TRUE", "FALSE"), "\n")
# In this subsample, individuals do not change groups over time and are never observed 
# in both treated and control groups, which provides a clear treated/control comparison 
# for the subsequent difference-in-differences estimate


# To assess the credibility of our difference-in-differences strategy, we examine 
# whether treated and control units look comparable before the policy is implemented. 
# We therefore report a pre-treatment balance table (≤ 2011) for the selected sample 
# (selection = 1), comparing baseline demographic characteristics, individual outcomes, 
# and collaboration measures. 
# The table reports mean values in treated and control groups, their difference (T–C), 
# and the associated p-value from a mean-comparison test. This exercise does not 
# prove the parallel-trends assumption, but it helps identify potential baseline 
# imbalances that should be kept in mind when interpreting the DiD results.

df_ind[selection == 1 & year <= 2011 & !is.na(group_treated),
       .(group_treated = max(group_treated, na.rm = TRUE),
         female = mean(female == 1, na.rm = TRUE),
         core = mean(core == 1, na.rm = TRUE),
         age = mean(age, na.rm = TRUE),
         outcome_1 = mean(outcome_1, na.rm = TRUE),
         outcome_2 = mean(outcome_2, na.rm = TRUE),
         outcome_3 = mean(outcome_3, na.rm = TRUE),
         field_1 = mean(field_1 == 1, na.rm = TRUE),
         field_2 = mean(field_2 == 1, na.rm = TRUE),
         field_3 = mean(field_3 == 1, na.rm = TRUE),
         field_4 = mean(field_4 == 1, na.rm = TRUE),
         field_5 = mean(field_5 == 1, na.rm = TRUE),
         field_6 = mean(field_6 == 1, na.rm = TRUE),
         field_7 = mean(field_7 == 1, na.rm = TRUE),
         field_8 = mean(field_8 == 1, na.rm = TRUE),
         link_ind = mean(link_ind, na.rm = TRUE),
         nb_partners = mean(nb_partners, na.rm = TRUE),
         total_collab = mean(total_collab, na.rm = TRUE),
         n_internal_partners = mean(n_internal_partners, na.rm = TRUE),
         n_external_partners = mean(n_external_partners, na.rm = TRUE),
         internal_collab = mean(internal_collab, na.rm = TRUE),
         external_collab = mean(external_collab, na.rm = TRUE)
       ), by = id] |>
  (\(id_pre) {
    
    vars <- c("female","core","age",
              "outcome_1","outcome_2","outcome_3",
              "field_1","field_2","field_3","field_4",
              "field_5","field_6","field_7","field_8",
              "link_ind","nb_partners","total_collab",
              "n_internal_partners","n_external_partners",
              "internal_collab","external_collab")
    
    title <- c(female = "Share of females",
               core = "Share of core members",
               age = "Mean age",
               outcome_1 = "Mean outcome 1",
               outcome_2 = "Mean outcome 2",
               outcome_3 = "Mean outcome 3",
               field_1 = "Share in field 1",
               field_2 = "Share in field 2",
               field_3 = "Share in field 3",
               field_4 = "Share in field 4",
               field_5 = "Share in field 5",
               field_6 = "Share in field 6",
               field_7 = "Share in field 7",
               field_8 = "Share in field 8",
               link_ind = "Share with any collaboration",
               nb_partners = "Mean number of partners",
               total_collab = "Mean total collaboration volume",
               n_internal_partners = "Mean number of internal partners",
               n_external_partners = "Mean number of external partners",
               internal_collab = "Mean internal collaboration volume",
               external_collab = "Mean external collaboration volume")
    
    nT <- id_pre[group_treated == 1, .N]
    nC <- id_pre[group_treated == 0, .N]
    nTot <- nT + nC
    tab <- rbindlist(lapply(vars, function(v){
      xT <- id_pre[group_treated == 1, get(v)]
      xC <- id_pre[group_treated == 0, get(v)]
      muT <- mean(xT, na.rm = TRUE)
      muC <- mean(xC, na.rm = TRUE)
      b <- ifelse(is.na(muT) | abs(muT) < 1e-12, NA_real_, (muC - muT) / muT)
      p <- tryCatch(t.test(xT, xC)$p.value, error = function(e) NA_real_)
      
      data.table(Variable = unname(title[v]),
                 `Treated mean` = sprintf("%.3f", muT),
                 `Treated sd` = sprintf("%.3f", sd(xT, na.rm = TRUE)),
                 `Control mean` = sprintf("%.3f", muC),
                 `Control sd` = sprintf("%.3f", sd(xC, na.rm = TRUE)),
                 `Difference t-test (b)` = ifelse(is.na(b), "", sprintf("%.2f", b)),
                 `Difference t-test (p)` = ifelse(is.na(p), "", paste0("(", formatC(p, format="f", digits=2), ")")))
    }))
    obs_row <- data.table(Variable = "Observations",
                          `Treated mean` = as.character(nT),
                          `Treated sd` = "",
                          `Control mean` = as.character(nC),
                          `Control sd` = "",
                          `Difference t-test (b)` = as.character(nTot),
                          `Difference t-test (p)` = "")
    rbind(tab, obs_row, fill = TRUE)
  })() |>
  print()
# The pre-treatment table (≤ 2011, selection = 1) indicates that treated and control 
# units differ on several dimensions. 
# Gender composition is fairly similar : the share of females is 0.394 in treated 
# units versus 0.434 in controls, and the standardized difference is small (b = 0.10) 
# and not statistically significant (p = 0.27). 
# In contrast, treated units have a much higher share of core members (0.273 vs 0.140), 
# corresponding to a sizeable negative standardized difference (b = −0.49) and a 
# highly significant mean difference (p < 0.01). 
# Controls are also older on average (43.0 vs 40.5 years, b = 0.06, p < 0.01). 
# Moreover, baseline performance and collaboration outcomes are systematically 
# higher in the control group: outcomes 1–3 are larger (b between 0.45 and 0.75, 
# all significant), and collaboration activity is stronger, with more partners and 
# higher total volumes (b = 0.59 and 0.80). 
# This pattern holds for both internal and external collaborations, with particularly 
# large gaps for external partners and external volumes (b = 0.87 and 0.91). 
# So, treated and control units are not perfectly balanced in levels before treatment. 
# While this does not invalidate a Diff-in-Diff strategy, since the key requirement 
# is parallel trends rather than equal levels, it highlights the importance of testing 
# pre-trends using an event-study specification.



# Joanne's part 
df_work <- df[!is.na(group_treated_1) & !is.na(group_treated_2) &
                selection_1 == 1 & selection_2 == 1]

library(data.table)
library(ggplot2)
library(scales)
library(igraph)
library(data.table)
library(ggplot2)

# 
# Color palette + global ggplot theme
# 
pal <- c(
  "Male" = "#5A8FA0",
  "Female" = "#BFC5C7",
  "Non-core" = "#5A8FA0",
  "Core" = "#BFC5C7",
  "Pre-treatment" = "#BFC5C7",
  "Post-treatment" = "#5A8FA0"
)

theme_tap <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey35"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )


# High-level dataset audit




cat("\nnDATASET AUDIT\n")
cat("Rows:", nrow(df_work), "\n")
cat("Unique dyads:", uniqueN(df_work$id_dyad), "\n")
cat("Years:", min(df_work$year, na.rm=TRUE), "->", max(df_work$year, na.rm=TRUE), "\n")

# Missingness snapshot (top 20)
miss <- df_work[, lapply(.SD, \(x) mean(is.na(x))), .SDcols = names(df_work)]
miss_long <- melt(miss, measure.vars = names(miss), variable.name = "var", value.name = "missing_rate")
setorder(miss_long, -missing_rate)
print(head(miss_long, 20))

# Panel unbalanced-ness: years per dyad
dyad_T <- df_work[, .(T = uniqueN(year)), by = id_dyad]
cat("\nDyad panel length summary:\n")
print(summary(dyad_T$T))

# Histogram 
ggplot(dyad_T, aes(x = T)) +
  geom_histogram(
    bins = 25,
    fill = pal["Post-treatment"],
    color = "white",
    linewidth = 0.4
  ) +
  labs(
    title = "Unbalanced panel",
    subtitle = "Number of observed years per dyad",
    x = "Years observed for a dyad",
    y = "Count"
  ) +
  scale_x_continuous(breaks = sort(unique(dyad_T$T))) +
  theme_tap

# Since most dyads are observed both before and after 2012, the difference-in-differences 
# estimates rely largely on within-dyad variation rather than compositional changes in the network. 
# This reduces concerns about dynamic selection bias and strengthens the credibility of the identification strategy.
# (because most of the dyads are observed for 15 years) 
#
# Our identification strategy relies on a difference-in-differences framework
# with dyad and year fixed effects. We exploit variation over time and across
# treatment exposure while controlling for unobserved time-invariant heterogeneity.
# Under the parallel trends assumption, this allows us to interpret the estimated
# coefficient as a causal effect of the treatment on collaboration formation.






# Core descriptive stats on links

cat("\n\nLink Descriptives\n\n")
cat("Overall link rate (mean(link_dyad)):", mean(df_work$link_dyad, na.rm=TRUE), "\n")
cat("Overall mean yearly_collabs_dyad:", mean(df_work$yearly_collabs_dyad, na.rm=TRUE), "\n")

# Link rate by year
link_year <- df_work[, .(
  link_rate = mean(link_dyad, na.rm=TRUE),
  mean_collabs = mean(yearly_collabs_dyad, na.rm=TRUE),
  N = .N
), by = year][order(year)]

# 2.1 Probability of link by year
ggplot(link_year, aes(x = year, y = link_rate)) +
  geom_line(linewidth = 1.1, color = pal["Post-treatment"]) +
  geom_point(size = 2.4, color = pal["Post-treatment"]) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = link_year$year) +
  labs(
    title = "Probability of link by year (dyad-level)",
    subtitle = "Share of dyads with at least one collaboration link",
    x = "Year",
    y = "Dyads with a link (%)"
  ) +
  theme_tap

# 2.2 Average collaboration intensity by year
ggplot(link_year, aes(x = year, y = mean_collabs)) +
  geom_line(linewidth = 1.1, color = pal["Post-treatment"]) +
  geom_point(size = 2.4, color = pal["Post-treatment"]) +
  scale_x_continuous(breaks = link_year$year) +
  labs(
    title = "Average collaboration intensity by year",
    subtitle = "Mean number of collaborations per dyad ",
    x = "Year",
    y = "Mean yearly_collabs_dyad"
  ) +
  theme_tap

# The average collaboration intensity as well as the probability of link per year per dyad exhibits a clear upward trend over time.
# Between 2006 and 2012, collaboration intensity increases substantially, suggesting
# a progressive densification of the network. After a slight temporary decline around
# some years, the upward trend resumes and reaches its highest level toward the end
# of the period.

# We can clearly see that the treatment had its effect as in 2012 we see a harsh upward trend>
# Towards the end the average collaboration shows a steady trend whereas the probability of 
# link formation by year reached its pic value in 2018 and it declined in 2019.
# showing maybe the beginning of a steady state. 

#
# But this pattern also indicates that collaboration activity was already increasing over time,
# independently of the treatment. Therefore, controlling for year fixed effects is
# essential to isolate the causal effect of the treatment from the general upward
# trend in network activity.
#
# In simple terms, year fixed effects control for shocks or trends that affect
# all dyads in a given year (e.g., overall growth in collaborations, policy changes,
# macro-environmental factors). They ensure that the estimated treatment effect
# is not driven by common time dynamics but by differences between treated and
# non-treated dyads over time.






# Treatment structure on dyads (none / one / both treated)


# Define dyad-treatment states (robust to NAs)
df_work[, treated_1 := fifelse(is.na(group_treated_1), 0L, as.integer(group_treated_1))]
df_work[, treated_2 := fifelse(is.na(group_treated_2), 0L, as.integer(group_treated_2))]

df_work[, dyad_treat := fifelse(treated_1 == 1 & treated_2 == 1, "both_treated",
                                fifelse(treated_1 + treated_2 == 1, "one_treated", "none_treated"))]
df_work[, dyad_treat := factor(dyad_treat, levels = c("none_treated","one_treated","both_treated"))]

# Core table: link rates by post x dyad_treat
tab_post <- df_work[, .(
  link_rate = mean(link_dyad, na.rm=TRUE),
  mean_collabs = mean(yearly_collabs_dyad, na.rm=TRUE),
  N = .N
), by = .(post, dyad_treat)][order(post, dyad_treat)]

print(tab_post)

tab_post[, post_lab := fifelse(as.integer(post) == 1, "Post-treatment", "Pre-treatment")]

ggplot(
  tab_post,
  aes(x = dyad_treat, y = link_rate,
      group = post_lab,
      color = post_lab,
      linetype = post_lab,
      shape = post_lab)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = pal[c("Pre-treatment","Post-treatment")]) +
  scale_linetype_manual(values = c("Pre-treatment" = "solid", "Post-treatment" = "solid")) +
  scale_shape_manual(values = c("Pre-treatment" = 16, "Post-treatment" = 17)) +
  labs(
    title = "Link rate by dyad treatment status",
    subtitle = "Pre-treatment vs Post-treatment",
    x = "Dyad treatment status",
    y = "Share with link",
    color = NULL, linetype = NULL, shape = NULL
  ) +
  theme_tap



# The probability of link formation increases after 2012 especially for the both_treated dyad type,
# whereas the non-treated dyads we see that it maintained its level of pre-treatment. 
# The magnitude of the increase appears relatively high  for the both_treated dyad type,
# indicating that the treatment has an immediate effect on the treated
# but also a slight indirect effect (spillover) on the untreated via the treated.
#






# Intra-group vs Extra-group dynamics



DT <- as.data.table(df_work)

DT[, post := as.integer(post)]
DT[, same_group := as.integer(same_group)]

# ---- Create both_treated and one_treated if needed ----
# If your columns are named differently, replace group_treated_1/2 accordingly.
DT[, treated_1 := as.integer(group_treated_1)]
DT[, treated_2 := as.integer(group_treated_2)]

DT[, both_treated := as.integer(treated_1 == 1 & treated_2 == 1)]
DT[, one_treated  := as.integer((treated_1 + treated_2) == 1)]

# Dyad treatment (3 categories)
DT[, dyad_treat := fifelse(both_treated == 1, "Both treated",
                           fifelse(one_treated  == 1, "One treated", "None treated"))]

DT[, dyad_treat := factor(dyad_treat, levels = c("None treated", "One treated", "Both treated"))]

# Aggregate data to compute the probability of link formation 
# (mean of the binary outcome) by:
# - dyadic treatment status
# - post-treatment period
# - intra-group (same_group = 1) vs extra-group (same_group = 0)
tab_inout <- DT[, .(
  link_rate = mean(link_dyad == 1, na.rm = TRUE),  # Binary outcome: link formation
  N = .N                                           # Number of observations in each cell
), by = .(dyad_treat, post, same_group)]



tab_inout[, post_lab := ifelse(as.integer(post) == 1, "Post-treatment", "Pre-treatment")]
tab_inout[, same_group := as.integer(same_group)]

ggplot(
  tab_inout,
  aes(x = dyad_treat, y = link_rate,
      group = post_lab,
      color = post_lab,
      linetype = post_lab,
      shape = post_lab)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.7) +
  facet_wrap(
    ~ same_group,
    labeller = labeller(same_group = c(`0` = "Extra-group", `1` = "Within-group"))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = pal[c("Pre-treatment", "Post-treatment")]) +
  scale_linetype_manual(values = c("Pre-treatment" = "solid", "Post-treatment" = "solid")) +
  scale_shape_manual(values = c("Pre-treatment" = 16, "Post-treatment" = 17)) +
  labs(
    title = "Before/After link probability by dyad treatment",
    subtitle = "Split by within-group vs extra-group dyads",
    x = "Dyad treatment status",
    y = "Share with link",
    color = NULL, linetype = NULL, shape = NULL
  ) +
  theme_tap


# Descriptive interpretation:
# The figure shows the evolution of link probabilities before and after the policy,
# distinguishing between extra-group and within-group dyads and by dyadic treatment status.

# Extra-group dyads:
# Before treatment, link probabilities decline sharply as we move from none treated
# to both treated dyads, indicating that cross-group collaborations involving treated
# units were already less frequent. After treatment, link probabilities become more
# compressed across treatment categories, with a clear drop for “none treated” dyads
# and relatively small differences between “one treated” and “both treated.”
# This suggests a general contraction in external collaborations, particularly for
# untreated pairs, while treated exposure does not substantially increase cross-group ties.

# Within-group dyads:
# Within groups, link probabilities are substantially higher overall.
# Before treatment, both untreated and treated dyads exhibit strong collaboration rates.
# After treatment, probability for fully treated dyads increased tremendously, indicating that
# internal group collaborations are more impacted.

# Economic intuition:
# The policy appears associated with a reallocation of collaboration patterns.
# External (extra-group) links weaken considerably, especially among untreated dyads,
# whereas within-group collaborations went up.
# This pattern is consistent with increased consolidation within groups
# following the policy, alongside a openness in cross-group interactions.



#Heterogeneity: Female-Female / Male-Male / Mixed



DT <- as.data.table(df_work)


DT[, post := as.integer(post)]
DT[, same_group := as.integer(same_group)]


DT[, `:=`(
  one_treated  = as.integer(abs(group_treated_1 - group_treated_2)),
  both_treated = as.integer(group_treated_1 * group_treated_2)
)]

DT[, dyad_treat := fifelse(both_treated == 1, "Both treated",
                           fifelse(one_treated == 1, "One treated", "None treated"))]

DT[, dyad_treat := factor(dyad_treat,
                          levels = c("None treated","One treated","Both treated"))]



DT[, dyad_gender := fifelse(female_1 == 1 & female_2 == 1, "F-F",
                            fifelse(female_1 == 0 & female_2 == 0, "M-M",
                                    "Mixed"))]

DT[, dyad_gender := factor(dyad_gender, levels = c("F-F","Mixed","M-M"))]


tab_gender <- DT[
  !is.na(dyad_gender),
  .(
    link_rate = mean(link_dyad == 1, na.rm = TRUE), 
    N = .N
  ),
  by = .(dyad_gender, dyad_treat, post, same_group)
]

tab_gender[, post_lab := ifelse(as.integer(post) == 1, "Post-treatment", "Pre-treatment")]
tab_gender[, same_group := as.integer(same_group)]

ggplot(
  tab_gender,
  aes(x = dyad_gender, y = link_rate,
      group = post_lab,
      color = post_lab,
      linetype = post_lab,
      shape = post_lab)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.6) +
  facet_grid(
    same_group ~ dyad_treat,
    labeller = labeller(same_group = c(`0` = "Extra-group", `1` = "Within-group"))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = pal[c("Pre-treatment", "Post-treatment")]) +
  scale_linetype_manual(values = c("Pre-treatment" = "solid", "Post-treatment" = "solid")) +
  scale_shape_manual(values = c("Pre-treatment" = 16, "Post-treatment" = 17)) +
  labs(
    title = "Gender composition heterogeneity in link formation",
    subtitle = "Before vs After (pre/post), split by within/extra-group and dyad treatment",
    x = "Dyad gender type",
    y = "Share with link",
    color = NULL, linetype = NULL, shape = NULL
  ) +
  theme_tap

# The treatment effect is mainly visible for dyads where both sides are treated.
# In that case, post-treatment extra-group links increase, especially for
# Mixed and M–M dyads, while F–F links remain more stable.

# This suggests that the policy encourages outward-oriented collaboration
# (cross-group expansion) rather than simple within-group reinforcement.

# Within groups, however, the changes are more modest across gender categories. 
# Link probabilities remain relatively stable overall, albeit with a slight upward shift 
# observed across all gender compositions (especially for the F-F link).

# Overall, the effect is heterogeneous by gender composition and driven
# primarily by treated–treated dyads expanding their external connections.

# Moreover it shows that the treatment has the most indirect effect on the mixed collaborations
# where one of them is treated.

# surprisingly for the none treated extra group the collaborations decreased in all 3 levels. 
# whereas withing group it stayed relatively stable (short increase for F-F).







#  Heterogeneity: Core-Core / Core-NonCore / NonCore-NonCore

DT <- as.data.table(df_work)
# Ensure required variables are properly typed
DT[, post := as.integer(post)]
DT[, same_group := as.integer(same_group)]

# Dyad treatment status )
if (!("one_treated" %in% names(DT)) || !("both_treated" %in% names(DT))) {
  DT[, `:=`(
    one_treated  = as.integer(abs(group_treated_1 - group_treated_2)),
    both_treated = as.integer(group_treated_1 * group_treated_2)
  )]
}

DT[, dyad_treat := fifelse(both_treated == 1, "Both treated",
                           fifelse(one_treated  == 1, "One treated", "None treated"))]
DT[, dyad_treat := factor(dyad_treat, levels = c("None treated", "One treated", "Both treated"))]

# Core composition of the dyad
DT[, dyad_core := fifelse(core_1 == 1 & core_2 == 1, "Core-Core",
                          fifelse(core_1 == 0 & core_2 == 0, "NonCore-NonCore",
                                  "Core-NonCore"))]
DT[, dyad_core := factor(dyad_core, levels = c("Core-Core", "Core-NonCore", "NonCore-NonCore"))]

# Build tab_core: link rate by core-type × dyad_treat × post × within/extra

tab_core <- DT[
  !is.na(dyad_core),
  .(
    link_rate = mean(link_dyad == 1, na.rm = TRUE),
    N = .N
  ),
  by = .(dyad_core, dyad_treat, post, same_group)
]
tab_core[, post_lab := ifelse(as.integer(post) == 1, "Post-treatment", "Pre-treatment")]
tab_core[, same_group := as.integer(same_group)]

ggplot(
  tab_core,
  aes(x = dyad_core, y = link_rate,
      group = post_lab,
      color = post_lab,
      linetype = post_lab,
      shape = post_lab)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.6) +
  facet_grid(
    same_group ~ dyad_treat,
    labeller = labeller(same_group = c(`0` = "Extra-group", `1` = "Within-group"))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = pal[c("Pre-treatment", "Post-treatment")]) +
  scale_linetype_manual(values = c("Pre-treatment" = "solid", "Post-treatment" = "solid")) +
  scale_shape_manual(values = c("Pre-treatment" = 16, "Post-treatment" = 17)) +
  labs(
    title = "Core status heterogeneity in link formation",
    subtitle = "Before vs After (pre/post), split by within/extra-group and dyad treatment",
    x = "Dyad core type",
    y = "Share with link",
    color = NULL, linetype = NULL, shape = NULL
  ) +
  theme_tap


# Summary table – Most salient patterns in link formation


summary_table <- data.frame(
  Treatment = c("None treated", "None treated",
                "One treated",  "One treated",
                "Both treated", "Both treated"),
  Group_level = c("Extra-group", "Within-group",
                  "Extra-group", "Within-group",
                  "Extra-group", "Within-group"),
  Most_flagrant_pattern = c(
    
    # None treated
    "No clear structural break; changes reflect a decrease in the collabs.",
    "Decline of core-core dominance; no endogenous strengthening of internal cohesion.",
    
    # One treated
    "Limited and heterogeneous adjustments; no strong outward expansion when only one side is exposed.",
    "Small or flat variations; weak evidence of spillover-driven internal restructuring.",
    
    # Both treated
    "Clear increase in extra-group links, especially among core-non core and noncore-noncore dyads (and male/mixed in gender split). Strong outward network expansion.",
    "Noticeable rise in core-non core and non core- non core activity; treated cores become more central in internal link formation."
  )
)

summary_table
# 

# Interpretation:
# The most economically meaningful changes occur when both sides are treated.
# In that configuration, the policy induces outward-oriented expansion
# (extra-group links) and activates core actors internally.
# Partial exposure (one treated) generates weak spillovers,
# while no-treatment cells mainly reflect secular trends.
# 

# Overall, the treatment appears to strengthen the role of core members as
# central drivers of link formation inside the group with non core members, consistent with a mechanism where
# treated cores increase network expansion rather than merely densifying
# peripheral ties.





# "Pre-trends" descriptive check 
#    Here: compare treated vs controls BEFORE 2012 on link rates, year by year.



DT <- as.data.table(df_work)  


DT[, year := as.integer(year)]
DT[, post := as.integer(post)]
DT[, same_group := as.integer(same_group)]


if (!("one_treated" %in% names(DT)) || !("both_treated" %in% names(DT))) {
  DT[, `:=`(
    one_treated  = as.integer(abs(group_treated_1 - group_treated_2)),
    both_treated = as.integer(group_treated_1 * group_treated_2)
  )]
}


DT[, dyad_treat := fifelse(both_treated == 1, "both_treated",
                           fifelse(one_treated  == 1, "one_treated", "none_treated"))]


DT_pre <- DT[year < 2012]


pre_trend <- DT_pre[, .(
  link_rate = mean(link_dyad == 1, na.rm = TRUE),
  N = .N
), by = .(year, dyad_treat, same_group)]


pre_trend[, dyad_treat := factor(dyad_treat,
                                 levels = c("none_treated","one_treated","both_treated"))]
pre_trend[, same_group := as.integer(same_group)]
pre_trend[, dyad_treat := factor(dyad_treat,
                                 levels = c("none_treated","one_treated","both_treated"))]

ggplot(
  pre_trend,
  aes(
    x = year, y = link_rate,
    group = dyad_treat,
    color = dyad_treat,
    linetype = dyad_treat,
    shape = dyad_treat
  )
) +
  geom_vline(xintercept = 2012, linetype = "dotted", linewidth = 0.6, alpha = 0.7) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.6) +
  facet_wrap(
    ~ same_group,
    labeller = labeller(same_group = c(`0` = "Extra-group", `1` = "Within-group"))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks = sort(unique(pre_trend$year))) +
  # Color = treated vs control (2-color palette)
  scale_color_manual(values = c(
    "none_treated" = pal["Pre-treatment"],
    "one_treated"  = pal["Post-treatment"],
    "both_treated" = pal["Post-treatment"]
  )) +
  # Linetype differentiates one_treated vs both_treated (while keeping same treated color)
  scale_linetype_manual(values = c(
    "none_treated" = "solid",
    "one_treated"  = "dashed",
    "both_treated" = "dotdash"
  )) +
  # Shape helps too (especially if printed in grayscale)
  scale_shape_manual(values = c(
    "none_treated" = 16,
    "one_treated"  = 17,
    "both_treated" = 15
  )) +
  labs(
    title = "Pre-treatment dynamics (year < 2012)",
    subtitle = "Controls vs treated dyads (one-treated vs both-treated distinguished by line/marker)",
    x = "Year",
    y = "Share with link",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  theme_tap +
  guides(
    color = guide_legend(order = 1, override.aes = list(linewidth = 1.05)),
    linetype = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  )


# Extra-group links
# None-treated dyads show a mild decline after 2007, while future treated
# dyads (one-treated and both-treated) display moderate upward movements.
# Importantly, in the immediate pre-period (2008–2011), trends evolve smoothly
# without sharp divergence or structural break. Differences are mainly in
# levels, not in slopes.

# Within-group links
# All categories exhibit gradual increases before 2012. Although treated
# dyads start from lower baseline shares, their trajectories remain broadly
# parallel to controls, with no visible anticipatory jump prior to treatment.

# Econometric implication
# The absence of clear pre-treatment divergence supports the credibility of
# the parallel trends assumption. Pre-2012 differences reflect baseline
# heterogeneity rather than differential dynamics, strengthening the causal
# interpretation of post-2012 effects.





# Network graph (for ONE group): before vs after
#    Choose a group id and plot only ACTIVE links (link_dyad==1)

plot_group_network <- function(dt, group_id, period = c("pre","post"),
                               same_group_only = TRUE,
                               layout_seed = 123,
                               vsize_range = c(5, 12),
                               edge_w_range = c(0.6, 4.0)) {
  period <- match.arg(period)
  sub <- copy(dt)
  
  sub <- sub[!is.na(group_id_1) & !is.na(group_id_2)]
  if (same_group_only) sub <- sub[same_group == 1]
  sub <- sub[group_id_1 == group_id & group_id_2 == group_id]
  sub <- sub[link_dyad == 1]
  
  if (period == "pre")  sub <- sub[year < 2012]
  if (period == "post") sub <- sub[year >= 2012]
  
  edges <- sub[, .(w = sum(yearly_collabs_dyad, na.rm = TRUE)), by = .(id_1, id_2)]
  edges <- edges[w > 0]
  
  if (nrow(edges) == 0) {
    message("No edges for group ", group_id, " in period ", period)
    return(invisible(NULL))
  }
  
  # Nodes: pull attributes from the dyadic panel (take first non-missing)
  nodes <- unique(rbindlist(list(
    sub[, .(id = id_1, female = female_1, core = core_1)],
    sub[, .(id = id_2, female = female_2, core = core_2)]
  ), use.names = TRUE, fill = TRUE))
  
  nodes <- nodes[, .(
    female = female[which(!is.na(female))[1]],
    core   = core[which(!is.na(core))[1]]
  ), by = id]
  
  nodes[, female_lab := fifelse(as.integer(female) == 1, "Female", "Male")]
  nodes[, core_lab   := fifelse(as.integer(core) == 1, "Core", "Non-core")]
  
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  
  # ---- Styling (palette) ----
  V(g)$color <- pal[V(g)$female_lab]                 # Male/Female colors
  V(g)$frame.color <- pal[V(g)$core_lab]             # Core/Non-core outline
  V(g)$label <- NA
  
  # Vertex size by core (or fallback)
  vsize <- ifelse(V(g)$core_lab == "Core", vsize_range[2], vsize_range[1])
  V(g)$size <- vsize
  
  # Edge width scaled by weight
  w <- igraph::E(g)$w
  w_scaled <- scales::rescale(log1p(w), to = edge_w_range)
  igraph::E(g)$width <- w_scaled
  igraph::E(g)$color <- "grey75"
  
  # Stable layout (same seed => comparable pre vs post)
  set.seed(layout_seed)
  lay <- igraph::layout_with_fr(g)
  
  # ---- Plot ----
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  par(mar = c(0, 0, 3.2, 0))
  plot(g,
       layout = lay,
       main = paste0("Group ", group_id, " — ", ifelse(period == "pre","Pre-treatment","Post-treatment")),
       vertex.label = NA,
       vertex.color = V(g)$color,
       vertex.frame.color = V(g)$frame.color,
       vertex.size = V(g)$size,
       edge.width = E(g)$width,
       edge.color = E(g)$color)
  
  # Legend: Sex (fill) + Core (border)
  legend("topleft", bty = "n", cex = 0.9,
         legend = c("Male", "Female"),
         pch = 21,
         pt.bg = c(pal["Male"], pal["Female"]),
         pt.cex = 1.2,
         col = "grey20")
  
  legend("bottomleft", bty = "n", cex = 0.9,
         legend = c("Non-core", "Core"),
         pch = 21,
         pt.bg = "white",
         col = c(pal["Non-core"], pal["Core"]),
         pt.cex = 1.2)
  
  invisible(g)
}

# Example
plot_group_network(df_work, group_id = "i13", period = "pre")
plot_group_network(df_work, group_id = "i13", period = "post")

# the network seems more dense + PYTHON CODE for Table 


# 
# 9) Which groups are the most "link-dense" pre vs post? 
# 
# Compute within-group link rate per group-year (only same_group dyads)
g_stats <- df_work[same_group == 1 & !is.na(group_id_1) & group_id_1 == group_id_2,
                   .(link_rate = mean(link_dyad, na.rm=TRUE),
                     mean_collabs = mean(yearly_collabs_dyad, na.rm=TRUE),
                     N = .N),
                   by = .(group = group_id_1, year, post)]

# Average pre vs post per group
g_prepost <- g_stats[, .(
  pre_link_rate  = mean(link_rate[post == 0], na.rm=TRUE),
  post_link_rate = mean(link_rate[post == 1], na.rm=TRUE),
  pre_N  = sum(N[post == 0], na.rm=TRUE),
  post_N = sum(N[post == 1], na.rm=TRUE)
), by = group]

g_prepost[, delta := post_link_rate - pre_link_rate]
setorder(g_prepost, -delta)

cat("\nTop 10 groups with largest increase in within-group link rate (post-pre):\n")
print(head(g_prepost, 10))

# Visual: top movers
topG <- head(g_prepost, 15)$group
tmp <- g_prepost[group %in% topG]

ggplot(tmp, aes(x = reorder(as.factor(group), delta), y = delta)) +
  geom_col(fill = pal["Post-treatment"], width = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = pal["Pre-treatment"]) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Top groups: change in within-group link rate",
    subtitle = "Post-treatment minus Pre-treatment (Δ link probability)",
    x = "Group",
    y = "Δ link probability"
  ) +
  theme_tap


# The change in within-group link probability varies substantially across groups.
# While some groups exhibit moderate increases, others experience very large
# gains in intra-group collaboration after 2012.
#
# This suggests significant heterogeneity in network dynamics at the group level.
# However, these differences reflect raw post-pre variations and do not yet
# isolate the causal impact of treatment.
#
# Further analysis is required to determine whether these changes are driven
# by treatment exposure or by pre-existing structural characteristics.





# TOP BETWEEN-GROUP PAIRS: change in extra-group link rate (post - pre) 

library(scales)

# Safety: ensure post & link_dyad exist
df_work[, post := as.integer(year >= 2012)]
df_work[, link_dyad := as.integer(yearly_collabs_dyad > 0)]
df_work[, link_dyad := as.integer(!is.na(yearly_collabs_dyad) & yearly_collabs_dyad > 0)]


# Keep only extra-group observations with valid group ids
dt_bg <- df_work[
  same_group == 0 &
    !is.na(group_id_1) & !is.na(group_id_2)
]

# Create an order-invariant group-pair id (A-B = B-A)
dt_bg[, group_pair := paste(pmin(group_id_1, group_id_2),
                            pmax(group_id_1, group_id_2), sep = " - ")]

# Compute pre vs post link rates for each group_pair
pair_prepost <- dt_bg[, .(
  pre_link_rate  = mean(link_dyad[post == 0], na.rm = TRUE),
  post_link_rate = mean(link_dyad[post == 1], na.rm = TRUE),
  pre_N  = sum(post == 0 & !is.na(link_dyad)),
  post_N = sum(post == 1 & !is.na(link_dyad)),
  N_total = sum(!is.na(link_dyad))
), by = group_pair]

# Drop pairs with no data on one side (cannot compute delta reliably)
pair_prepost <- pair_prepost[is.finite(pre_link_rate) & is.finite(post_link_rate)]

# Delta (post - pre)
pair_prepost[, delta := post_link_rate - pre_link_rate]

# keep only sufficiently observed pairs (avoid tiny-sample noise)
min_obs <- 200  # adjust if needed
pair_prepost_f <- pair_prepost[pre_N >= min_obs & post_N >= min_obs]

# Top 15 pairs with largest increase
topK <- 4
top_pairs <- pair_prepost_f[order(-delta)]
top_pairs <- head(top_pairs, topK)   

print(top_pairs)

# Plot
ggplot(top_pairs, aes(x = reorder(group_pair, delta), y = delta)) +
  geom_col(fill = pal["Post-treatment"], width = 0.75) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = pal["Pre-treatment"]) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Top group-pairs: change in extra-group link rate",
    subtitle = "Post-treatment minus Pre-treatment (Δ link probability)",
    x = "Group pair (A - B)",
    y = "Δ link probability"
  ) +
  theme_tap


# The leading groups may be:
# - scientifically complementary,
# - geographically close,
# - institutionally aligned,
# - or particularly responsive to the reform.
#
# The group pairs displaying the largest variations in link probability
# (post - pre) correspond to the inter-group relationships that experienced
# the strongest intensification after 2012. i13-i88. 
#
# However, these results reflect raw variations and do not yet formally
# identify a causal treatment effect.


# 
# 11) i13 -- i88 
# 

gt_g <- data.table::rbindlist(list(
  df[!is.na(group_id_1), .(group = as.character(group_id_1), treated = as.integer(group_treated_1))],
  df[!is.na(group_id_2), .(group = as.character(group_id_2), treated = as.integer(group_treated_2))]
))[, treated := data.table::fifelse(is.na(treated), 0L, treated)
][, .(treated = max(treated, na.rm = TRUE)), by = group]

gt_g[group %in% c("i13","i80")]



# The 2 groups are treated


library(data.table)
library(igraph)
library(scales)

pal <- c(
  "Male" = "#5A8FA0",
  "Female" = "#BFC5C7",
  "Non-core" = "#5A8FA0",
  "Core" = "#BFC5C7",
  "Pre-treatment" = "#BFC5C7",   
  "Post-treatment" = "#5A8FA0"   
)

plot_two_groups_network <- function(dt,
                                    g1 = "i13",
                                    g2 = "i80",
                                    period = c("pre", "post"),
                                    year_cutoff = 2012,
                                    layout_seed = 123,
                                    vsize_core = 14,
                                    vsize_noncore = 6,
                                    edge_w_range = c(0.8, 4)) {
  
  period <- match.arg(period)
  
  # ---- Subset
  sub <- copy(dt)
  sub <- sub[!is.na(group_id_1) & !is.na(group_id_2)]
  
  sub <- sub[
    (as.character(group_id_1) == g1 & as.character(group_id_2) == g2) |
      (as.character(group_id_1) == g2 & as.character(group_id_2) == g1)
  ]
  
  if (period == "pre")  sub <- sub[year < year_cutoff]
  if (period == "post") sub <- sub[year >= year_cutoff]
  
  sub <- sub[yearly_collabs_dyad > 0 | link_dyad == 1]
  
  if (nrow(sub) == 0) {
    message("No links found.")
    return(invisible(NULL))
  }
  
  # ---- EDGES
  edges <- sub[, .(
    from = trimws(as.character(id_1)),
    to   = trimws(as.character(id_2)),
    wraw = yearly_collabs_dyad
  )]
  
  edges <- edges[!is.na(from) & nzchar(from) & !is.na(to) & nzchar(to)]
  edges <- edges[from != to]
  edges[, `:=`(a = pmin(from, to), b = pmax(from, to))]
  edges <- edges[, .(w = sum(wraw, na.rm = TRUE)), by = .(a, b)]
  setnames(edges, c("a", "b"), c("from", "to"))
  edges <- edges[w > 0]
  
  # ---- Graph (safe)
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # ---- Nodes attributes
  nodes_raw <- rbindlist(list(
    sub[, .(name = trimws(as.character(id_1)),
            group = as.character(group_id_1),
            core  = as.integer(core_1))],
    sub[, .(name = trimws(as.character(id_2)),
            group = as.character(group_id_2),
            core  = as.integer(core_2))]
  ), use.names = TRUE, fill = TRUE)
  
  nodes_raw <- nodes_raw[!is.na(name) & nzchar(name)]
  
  
  nodes <- nodes_raw[, .(
    group = group[which(!is.na(group))[1]],
    core  = max(core, na.rm = TRUE)
  ), by = name]
  
  nodes[is.infinite(core), core := 0L]
  nodes[is.na(core), core := 0L]
  
  nodes <- unique(nodes, by = "name")
  idx <- match(V(g)$name, nodes$name)
  
  V(g)$group <- nodes$group[idx]
  V(g)$core  <- nodes$core[idx]
  
  
  V(g)$color <- ifelse(V(g)$group == g1, pal["Post-treatment"], pal["Pre-treatment"])
  
  
  V(g)$shape <- "circle"
  
  
  V(g)$size <- ifelse(V(g)$core == 1, vsize_core, vsize_noncore)
  
  V(g)$frame.color <- "grey20"
  V(g)$label <- NA
  
  # edges
  E(g)$width <- scales::rescale(log1p(E(g)$w), to = edge_w_range)
  E(g)$color <- "grey70"
  
  set.seed(layout_seed)
  lay <- layout_with_fr(g)
  
  par(mar = c(0, 0, 3, 0))
  
  plot(
    g,
    layout = lay,
    main = paste0(g1, " ↔ ", g2, " — ", ifelse(period == "pre", "Pre", "Post")),
    vertex.color = V(g)$color,
    vertex.size = V(g)$size,
    edge.width = E(g)$width,
    edge.color = E(g)$color
  )
  
  
  legend("topleft",
         legend = c(g1, g2),
         pch = 21,
         pt.bg = c(pal["Post-treatment"], pal["Pre-treatment"]),
         pt.cex = 1.5,
         bty = "n")
  
  
  legend("bottomleft",
         legend = c("Non-core", "Core"),
         pch = 21,
         pt.bg = "white",
         pt.cex = c(1.0, 1.8),
         bty = "n")
  
  invisible(g)
}

# Usage
plot_two_groups_network(df_work, "i13", "i80", "pre")
plot_two_groups_network(df_work, "i13", "i80", "post")


# Comparing the pre- and post-treatment networks between i13 and i80,
# we observe a clear structural transformation. In the pre-period, cross-group
# collaboration is sparse and fragmented, with only small isolated components
# and no dominant bridging actors. Core members are present but do not play
# a central inter-group role.

# In the post-period, the network between the 2 grps becomes substantially denser and more
# integrated. A large connected component emerges, and core members
# (larger nodes) occupy central positions, forming multiple ties with
# opposite-group members. They act as bridges that effectively connect
# the two communities.

# This pattern indicates that the integration of the two treated groups
# is core-driven: core actors expand cross-group collaborations and
# structure the inter-group linkage process. The merger does not simply
# increase links mechanically; it activates central members as brokers,
# transforming two previously fragmented groups into a more cohesive
# inter-group network.








###############################################################################.
## II - Impact of treatment on individual outcomes ----
###############################################################################.
# Cêlestine's part 

# Main DID specification (baseline):
# y_{i,g,t} = alpha + beta(T_g x Post_t) + delta_t + theta_i + epsilon_{i,g,t}
# where:
# - i indexes individuals, t indexes years, and g is the (time-invariant) group of individual i
# - T_g is an indicator equal to 1 if the individual's group is treated by the policy
# - Post_t is an indicator equal to 1 for years after the policy start (post 2012)
# - delta_t are year fixed effects (capture common shocks/trends affecting everyone in a given year).
# - theta_i are individual fixed effects (capture all time-invariant individual characteristics).

# In fixest, this is written as:
# feols(y ~ group_treated:post | id + year, data = df_sel, vcov = ~ group_id)

# Explanation of each element:
# - feols(...): fits an OLS regression with high-dimensional fixed effects
# - y ~ group_treated:post >> the regressor of interest is the interaction Treated x Post
# - |id + year >> fixed effects part of the model
# - vcov = ~ group_id >> standard errors are clustered at the group level, because 
#                        treatment is assigned at the group level and outcomes may 
#                        be correlated among individuals within the same group.

# Interpretation of beta :
# Beta measures the average post-policy change in treated groups relative to the 
# contemporaneous change in control groups.
# Identification relies on the parallel-trends assumption: absent treatment, treated 
# and control groups would have followed similar trends over time


### II.1 - Average impact on members of treated groups #########################

#### a) Difference-in-difference -----------------------------------------------
##### a.1. Diff-in-diff on individual outcomes ----
# These diff-in-diff estimates are obtained on the selected sample (selection = 1) 
# with individual and year fixed effects, and standard errors clustered at the group 
# level (the treatment assignment level). 

out1 <- feols(outcome_1 ~ group_treated:post + age | id  + year,
              data = df_sel, vcov = ~ group_id)
# Age is collinear with individual and year fixed effects (age increases mechanically 
# with time), so fixest drops it. 
# We therefore keep the baseline specification without age
out1 <- feols(outcome_1 ~ group_treated:post | id  + year, 
              data = df_sel, vcov = ~ group_id)

out2 <- feols(outcome_2 ~ group_treated:post | id  + year,
              data = df_sel, vcov = ~ group_id)

out3 <- feols(outcome_3 ~ group_treated:post | id  + year,
              data = df_sel, vcov = ~ group_id)

etable(out1, out2, out3, keep = "%group_treated:post")

means_all  <- c(mean(df_sel$outcome_1, na.rm = TRUE),
                mean(df_sel$outcome_2, na.rm = TRUE),
                mean(df_sel$outcome_3, na.rm = TRUE))
means_pre  <- c(mean(df_sel[post == 0, outcome_1], na.rm = TRUE),
                mean(df_sel[post == 0, outcome_2], na.rm = TRUE),
                mean(df_sel[post == 0, outcome_3], na.rm = TRUE))
means_post <- c(mean(df_sel[post == 1, outcome_1], na.rm = TRUE),
                mean(df_sel[post == 1, outcome_2], na.rm = TRUE),
                mean(df_sel[post == 1, outcome_3], na.rm = TRUE))

# Betas and ratios
beta <- c(coef(out1)["group_treated:post"],
          coef(out2)["group_treated:post"],
          coef(out3)["group_treated:post"])

pct_pre  <- 100 * beta /means_pre

data.table(outcome = c("outcome_1","outcome_2","outcome_3"),
           beta = as.numeric(beta),
           mean_pre  = means_pre,
           mean_post = means_post,
           mean_all = means_all,
           pct_pre = pct_pre)
# For outcome_1, the coefficient on treated × post is −0.095 (s.e 0.154), which 
# is not statistically significant. In magnitude, it corresponds to about −3.6% of 
# the pre-treatment mean (2.67). 
# For outcome_2, the estimated effect is −1.43 (s.e 10.97), also not statistically 
# significant and very imprecise, corresponding to roughly −5.5% of the pre-treatment 
# mean (26.32). 
# For outcome_3, the coefficient is 0.316 (s.e 0.266), not statistically significant.
# Relative to the pre-treatment mean (2.85) this represents about +11.1%. 
# These specifications do not provide strong statistical evidence of an
# average post-policy effect on the three individual outcomes, which motivates 
# examining dynamic effects and pre-trends using an event-study approach.


##### a.2. Diff-in-diff on collaboration outcomes ----
# Collaboration outcomes :
# - link_ind : indicator of having at least one collaboration link (or link existence)
# - nb_partners : number of distinct partners
# - total_collab : total collaboration volume
# - n_internal_partners/n_external_partners : number of internal/external partners
# - internal_collab/external_collab : volume of internal/external collaborations

link <- feols(link_ind ~ group_treated:post | id + year,
              data = df_sel, vcov = ~ group_id)

nbpart <- feols(nb_partners ~ group_treated:post | id + year,
                data = df_sel, vcov = ~ group_id)

totcollab <- feols(total_collab ~ group_treated:post | id + year,         
                   data = df_sel, vcov = ~ group_id)

int_part <- feols(n_internal_partners ~ group_treated:post | id + year,
                  data = df_sel, vcov = ~ group_id)

ext_part <- feols(n_external_partners ~ group_treated:post | id + year,
                  data = df_sel, vcov = ~ group_id)

int_vol <- feols(internal_collab ~ group_treated:post | id + year,
                 data = df_sel, vcov = ~ group_id)

ext_vol <- feols(external_collab ~ group_treated:post | id + year,
                 data = df_sel, vcov = ~ group_id)

etable(link, nbpart, totcollab, int_part, ext_part, int_vol, ext_vol,
       keep = "%group_treated:post")

collab_vars <- c("link_ind", "nb_partners", "total_collab",
                 "n_internal_partners", "n_external_partners",
                 "internal_collab", "external_collab")

models <- list(link, nbpart, totcollab, int_part, ext_part, int_vol, ext_vol)

beta_c <- sapply(models, function(m) as.numeric(coef(m)["group_treated:post"]))

means_pre_c <- sapply(collab_vars, function(v) mean(df_sel[post == 0, get(v)], na.rm = TRUE))
means_post_c <- sapply(collab_vars, function(v) mean(df_sel[post == 1, get(v)], na.rm = TRUE))
means_all_c <- sapply(collab_vars, function(v) mean(df_sel[[v]], na.rm = TRUE))

pct_pre_c  <- 100 * beta_c / means_pre_c

data.table(outcome = collab_vars,
           beta = beta_c,
           mean_pre = means_pre_c,
           mean_post = means_post_c,
           mean_all = means_all_c,
           pct_pre = pct_pre_c)

# These Diff-in-Diff estimates focus on collaboration outcomes (selection = 1) with 
# individual and year fixed effects, and standard errors clustered at the group level. 
# The probability of having any collaboration (link_ind) increases by 0.029 (s.e 0.016), 
# which is marginally significant and corresponds to about +5.3% of the pre-treatment 
# mean (0.538). 
# The policy is associated with a clear rise in the number of partners: +0.494 
# (s.e. 0.189), around +25.1% of the pre-treatment mean (1.97). 
# Total collaboration volume (total_collab) also increases by +0.708 (s.e 0.328), 
# about +23.4% relative to the pre-treatment mean (3.02). 
# Decomposing collaborations, the effect is concentrated on the internal margin : 
# internal partners increase by +0.540 (s.e. 0.157), roughly +49.1%, and internal 
# collaboration volume increases strongly by +1.139 (s.e 0.294), about +61.3% of 
# the pre-treatment mean (1.86). By contrast, the external margin does not increase : 
# the coefficient on external partners is small and not significant (−0.046), while 
# external collaboration volume decreases by −0.431 (s.e 0.148), i.e. about −37.0% 
# of the pre-treatment mean (1.16). 
# The Diff-in-Diff results suggest that the policy mainly strengthens 
# within-group collaborations, with little evidence of an expansion in external 
# partnering and some evidence of a substitution away from external collaboration volume.

library(data.table)
library(ggplot2)

plot_did_estimand_clean <- function(dt, y,
                                    treated_var="group_treated",
                                    post_var="post",
                                    treated_label="Treated",
                                    control_label="Untreated",
                                    beta_hat=NULL,
                                    digits=2,
                                    title=NULL) {
  d <- copy(as.data.table(dt))
  d <- d[!is.na(get(y)) & !is.na(get(treated_var)) & !is.na(get(post_var))]
  d[, period := fifelse(get(post_var)==0, "Pre-treatment", "Post-treatment")]
  d[, group  := fifelse(get(treated_var)==1, treated_label, control_label)]
  d[, period := factor(period, levels=c("Pre-treatment","Post-treatment"))]
  d[, group  := factor(group,  levels=c(control_label, treated_label))]
  
  # Means
  m <- d[, .(mean_y = mean(get(y), na.rm=TRUE)), by=.(group, period)]
  mu_T_pre  <- m[group==treated_label & period=="Pre-treatment", mean_y]
  mu_T_post <- m[group==treated_label & period=="Post-treatment",  mean_y]
  mu_C_pre  <- m[group==control_label & period=="Pre-treatment", mean_y]
  mu_C_post <- m[group==control_label & period=="Post-treatment",  mean_y]
  
  cf_T_post <- mu_T_pre + (mu_C_post - mu_C_pre)
  did_means <- (mu_T_post - mu_T_pre) - (mu_C_post - mu_C_pre)
  
  # Data for plotting (3 series)
  df_lines <- rbind(
    data.table(series="Control (solid)",  period=c("Pre-treatment","Post-treatment"), mean_y=c(mu_C_pre, mu_C_post)),
    data.table(series="Treated (solid)",  period=c("Pre-treatment","Post-treatment"), mean_y=c(mu_T_pre, mu_T_post)),
    data.table(series="Counterfactual (dashed)", period=c("Pre-treatment","Post-treatment"), mean_y=c(mu_T_pre, cf_T_post))
  )
  df_lines[, period := factor(period, levels=c("Pre-treatment","Post-treatment"))]
  
  # Points only for real means
  df_pts <- df_lines[series %in% c("Control (solid)","Treated (solid)")]
  df_pts[, label := formatC(mean_y, format="f", digits=digits)]
  
  # Legend aesthetics
  cols <- c("Control (solid)" = "#BFC5C7",
            "Treated (solid)" = "#5A8FA0",
            "Counterfactual (dashed)" = "#5A8FA0")
  ltys <- c("Control (solid)" = "solid",
            "Treated (solid)" = "solid",
            "Counterfactual (dashed)" = "dashed")
  
  subtitle_txt <- if (!is.null(beta_hat)) {
    sprintf("DiD =  %.3f (means) | FE beta =  %.3f ", did_means, beta_hat)
  } else {
    sprintf("DiD =  %.3f (means) ", did_means)
  }
  
  ggplot(df_lines, aes(x=period, y=mean_y, color=series, linetype=series, group=series)) +
    geom_line(linewidth=1.2) +
    # Points for real means
    geom_point(data=df_pts, size=2.8) +
    
    # Numeric mean labels
    geom_text(data = df_pts[period == "Pre-treatment"],
              aes(label = label),
              nudge_x = -0.06, hjust = 1, vjust = 0.5,
              size = 3.2, show.legend = FALSE) +
    geom_text(data = df_pts[period == "Post-treatment"],
              aes(label = label),
              nudge_x =  0.06, hjust = 0, vjust = 0.5,
              size = 3.2, show.legend = FALSE) +
    
    # Diff-in-Diff vertical gap
    geom_segment(aes(x="Post-treatment", xend="Post-treatment", y=cf_T_post, yend=mu_T_post),
                 inherit.aes = FALSE, linetype="dashed") +
    annotate("text", x="Post-treatment", y=(cf_T_post+mu_T_post)/2,
             label="DiD", hjust=-0.2, size=3.3) +
    
    scale_color_manual(values=cols) +
    scale_linetype_manual(values=ltys) +
    labs(title = ifelse(is.null(title), paste("DiD estimand:", y), title),
         subtitle = subtitle_txt,
         x="Period", y=y,
         color = NULL, linetype = NULL) +
    theme_classic() +
    theme(legend.position="right")
}

sig_vars_5pct <- c("nb_partners", "total_collab",
                   "n_internal_partners", "internal_collab",
                   "external_collab")

beta_map <- c(nb_partners = 0.4944,
              total_collab = 0.7080,
              n_internal_partners = 0.5404,
              internal_collab = 1.1390,
              external_collab = -0.4310)

plots <- lapply(sig_vars_5pct, function(v){
  plot_did_estimand_clean(df_sel, v, beta_hat = beta_map[v])
})

for(p in plots) print(p)


#### b) Event-study ------------------------------------------------------------
# To go beyond the average post-treatment effect captured by the baseline Diff-in-Diff, 
# we estimate an event-study specification that traces the dynamic impact of the 
# policy over time. This approach interacts treatment status with year indicators 
# (relative to the policy start) and includes individual and year fixed effects, 
# with standard errors clustered at the group level. The event study serves two purposes : 
# (i) it provides a year-by-year profile of treatment effects after 2012 and 
# (ii) it allows us to assess the plausibility of the parallel-trends assumption 
#      by checking whether the pre-treatment coefficients are close to zero and 
#      jointly insignificant.

col_pre  <- "#BFC5C7"
col_post <- "#5A8FA0"

# Reference year: last pre-policy year (post == 0)
ref_year <- df_sel[post == 0, max(year, na.rm = TRUE)]
df_es <- copy(df_sel)

# Reference year: last pre-policy year (post == 0). This year is normalized to 0 in the plot
post_start <- ref_year + 1


##### b.1. Event-study on individual outcomes ----
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), oma = c(0, 0, 0, 0))

plot_es <- function(outcome_var, main_title, show_legend = TRUE){
  fml <- as.formula(paste0(outcome_var, " ~ i(year, group_treated, ref = ", ref_year, ") | id + year"))
  es_m <- feols(fml, data = df_es, vcov = ~ group_id)
  ct <- coeftable(es_m)
  keep <- grepl("^year::[0-9]{4}:group_treated$", rownames(ct))
  es_df <- data.table(
    year = as.integer(sub("^year::([0-9]{4}):group_treated$", "\\1", rownames(ct)[keep])),
    beta = ct[keep, "Estimate"],
    se   = ct[keep, "Std. Error"]
  )
  es_df[, `:=`(ci_low = beta - 1.96*se, ci_high = beta + 1.96*se)]
  es_df <- rbind(es_df, data.table(year = ref_year, beta = 0, se = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  setorder(es_df, year)
  is_post <- es_df$year >= post_start
  ylim <- range(c(es_df$ci_low, es_df$ci_high, es_df$beta), na.rm = TRUE)
  plot(es_df$year, es_df$beta, type = "n", ylim = ylim,
       xlab = "", ylab = "ATT", main = main_title)
  abline(h = 0, lty = 2)
  segments(es_df$year[!is_post], es_df$ci_low[!is_post], es_df$year[!is_post], es_df$ci_high[!is_post],
           col = adjustcolor(col_pre,  alpha.f = 0.35), lwd = 2)
  segments(es_df$year[ is_post], es_df$ci_low[ is_post], es_df$year[ is_post], es_df$ci_high[ is_post],
           col = adjustcolor(col_post, alpha.f = 0.35), lwd = 2)
  for(i in 1:(nrow(es_df) - 1)){
    seg_col <- ifelse(es_df$year[i] >= post_start, col_post, col_pre)  # 2011-2012 gris
    segments(es_df$year[i], es_df$beta[i], es_df$year[i + 1], es_df$beta[i + 1],
             col = seg_col, lwd = 2)
  }
  points(es_df$year[!is_post], es_df$beta[!is_post], pch = 16, col = col_pre)
  points(es_df$year[ is_post], es_df$beta[ is_post], pch = 16, col = col_post)
  if(show_legend){
    legend("topleft", legend = c("Pre-treatment", "Post-treatment"),
           col = c(col_pre, col_post), pch = 16, lwd = 2, bty = "n")
  }
  invisible(es_m)
}

m1 <- plot_es("outcome_1", "Event-study : outcome_1", show_legend = FALSE)
m2 <- plot_es("outcome_2", "Event-study : outcome_2", show_legend = FALSE)
m3 <- plot_es("outcome_3", "Event-study : outcome_3", show_legend = TRUE)

par(op)
# Across the three outcomes, the pre-treatment coefficients display noticeable 
# year-to-year variation and are not consistently centered around zero, suggesting 
# the presence of pre-period dynamics that should be kept in mind when interpreting 
# post-policy estimates. Post-treatment effects are generally positive for outcomes 
# 1 and 3 in several years but remain imprecisely estimated, with confidence intervals 
# often overlapping zero. 
# Outcome 2 shows particularly large volatility and wide confidence intervals, 
# indicating substantial noise and limited precision. 
# The event-study plots suggest modest and unstable post-treatment patterns, and 
# the evidence on parallel trends appears mixed, motivating cautious interpretation




##### b.2. Event-study on collaboration outcomes ----
op <- par(no.readonly = TRUE)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
m_link <- plot_es("link_ind", "Event-study: link_ind", show_legend = FALSE)
m_nbpart <- plot_es("nb_partners", "Event-study: nb_partners", show_legend = FALSE)
m_tot <- plot_es("total_collab", "Event-study: total_collab", show_legend = TRUE)
# The event-study plots for collaboration outcomes suggest a broadly positive post-policy 
# dynamic for the intensive margin of collaboration. For nb_partners and total_collab, 
# post-treatment coefficients are mostly positive over several years after 2012, 
# consistent with an increase in the number of distinct partners and in total 
# collaboration volume for treated groups relative to controls. link_ind also shows 
# positive post-treatment estimates, indicating a higher probability of having at 
# least one collaboration, although the magnitude varies across years. 
# At the same time, the pre-treatment coefficients exhibit some variation and are 
# not uniformly close to zero, which means pre-period dynamics cannot be fully ruled out. 
# Finally, the last year displays a sharp drop with very wide confidence intervals 
# in some panels, suggesting that this point is noisy and should not drive conclusions. 

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
m_intp <- plot_es("n_internal_partners", "Event-study: internal partners", show_legend = FALSE)
m_extp <- plot_es("n_external_partners", "Event-study: external partners", show_legend = TRUE)
# The event-study decomposition highlights that the post-policy increase in partnering 
# is driven primarily by internal partners. After 2012, the coefficients for internal 
# partners are consistently positive and often sizeable, suggesting that treated groups 
# experience a sustained rise in within-group partnering relative to controls. 
# In contrast, the dynamics for external partners are more muted: post-treatment 
# estimates are positive in some years but generally smaller and less stable, with 
# confidence intervals frequently overlapping zero. The final year shows a sharp 
# negative point estimate with a very wide confidence interval, indicating that 
# this last observation is noisy and should not drive inference. 
# These patterns are consistent with the policy strengthening collaboration within 
# treated groups more than expanding collaborations outside the group.

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
m_intv <- plot_es("internal_collab", "Event-study: internal collab", show_legend = FALSE)
m_extv <- plot_es("external_collab", "Event-study: external collab", show_legend = TRUE)
# The event-study patterns for collaboration volumes reinforce the internal-versus-external 
# contrast. For internal collaboration volume, post-treatment coefficients are 
# consistently positive and relatively large over most years after 2012, suggesting 
# a sustained increase in within-group collaboration intensity among treated units 
# compared with controls. In contrast, external collaboration volume shows smaller 
# and less stable post-treatment estimates, often close to zero, and the final year 
# features a sharp negative point estimate with an extremely wide confidence 
# interval—indicating a highly noisy observation that should not drive conclusions. 
# These dynamics are consistent with the policy primarily boosting collaboration 
# within treated groups rather than expanding collaboration volume outside the group, 
# and potentially even substituting away from external collaboration in some years
par(op)


### II.2 - Average impact on untreated pre-collaborators of treated members ####
# This section examines whether the policy generates indirect effects (spillovers) 
# on individuals whose group is not treated. The underlying idea is that a targeted 
# funding shock may propagate through the collaboration network and affect non-treated 
# individuals who were already connected, prior to the reform, to members of treated 
# groups. We therefore measure each non-treated individual’s pre-policy exposure 
# (either a dummy for any treated collaborator, or the number of distinct treated 
# collaborators) and compare post-policy changes for exposed versus non-exposed 
# non-treated individuals.

# Keep only pre-policy collaboration links to build exposure
pre_dt <- dt_long_partner[link_dyad == 1 & # active collaboration link
                            same_group == 0 & # outside own group
                            year <= ref_year] # pre-policy only

# Build several pre-policy exposure measures
expo_pre <- pre_dt[, .(
  # exposed_pre_any = 1 if i had at least one treated collaborator pre-policy (outside-group links)
  exposed_pre_any = as.integer(any(partner_treated == 1, na.rm = TRUE)),
  
  # pre_n_treated_partners counts distinct treated partners
  pre_n_treated_partners = uniqueN(partner[partner_treated == 1])
  
), by = .(id, group_id)]

# df_ind contains all years (pre + post)
df_ind <- merge(df_ind, expo_pre, by = c("id","group_id"), all.x = TRUE)

# People with no pre-policy outside-group links get NA after merge set to 0.
vars_expo <- c("exposed_pre_any","pre_n_treated_partners")
for (v in vars_expo) df_ind[is.na(get(v)), (v) := 0]

df_sel_spill <- df_ind[selection == 1]

# Diff-in-Diff estimation on non-treated individuals only
run_spill <- function(y, expo_var){
  feols(as.formula(paste0(y, " ~ post:", expo_var, " | id + year")),
        data = df_sel_spill[group_treated == 0],
        vcov = ~ group_id)
}

# Variables to analyze
ind_outcomes <- c("outcome_1","outcome_2","outcome_3")
collab_vars  <- c("link_ind","nb_partners","total_collab",
                  "n_internal_partners","n_external_partners",
                  "internal_collab","external_collab")

expo_list <- c("exposed_pre_any", # ever exposed pre-policy
               "pre_n_treated_partners") # distinct treated partners

for (e in expo_list) {
  # Individual outcomes
  mods_ind <- lapply(ind_outcomes, run_spill, expo_var = e)
  names(mods_ind) <- ind_outcomes
  print(etable(mods_ind, keep = paste0("%post:", e, "$")))
  # Collaboration outcomes
  mods_col <- lapply(collab_vars, run_spill, expo_var = e)
  names(mods_col) <- collab_vars
  print(etable(mods_col, keep = paste0("%post:", e, "$")))
}

# Overall, the evidence points to negative spillovers. For individual outcomes, 
# we find no significant effects on outcome_1 and outcome_2, while outcome_3 decreases 
# significantly for pre-exposed non-treated individuals (−1.644**), and the decline 
# becomes larger with exposure intensity (−0.223*** per additional distinct treated 
# partner). On collaboration outcomes, pre-policy exposure is associated with a 
# substantial contraction in activity: the probability of having any collaboration 
# (link_ind) falls sharply (−0.185***), the number of partners declines (especially 
# in the intensity specification), and internal collaboration volume decreases 
# significantly, whereas “external” outcomes do not display robust changes.




### II.3 - Heterogeneity  ######################################################
did_by_subsample <- function(y, dt){
  feols(as.formula(paste0(y, " ~ group_treated:post | id + year")),
        data = dt, vcov = ~ group_id)
}

#### a) Heterogeneity by gender ------------------------------------------------
##### a.1. Direct effect ----
# Men (female == 0)
mods_men_out <- lapply(ind_outcomes, \(y) did_by_subsample(y, df_sel[female == 0]))
names(mods_men_out) <- ind_outcomes
mods_men_col <- lapply(collab_vars, \(y) did_by_subsample(y, df_sel[female == 0]))
names(mods_men_col) <- collab_vars

# Women (female == 1)
mods_women_out <- lapply(ind_outcomes, \(y) did_by_subsample(y, df_sel[female == 1]))
names(mods_women_out) <- ind_outcomes
mods_women_col <- lapply(collab_vars, \(y) did_by_subsample(y, df_sel[female == 1]))
names(mods_women_col) <- collab_vars

cat("\nMen \n")
etable(mods_men_out, keep = "%group_treated:post$")
etable(mods_men_col, keep = "%group_treated:post$")

cat("\nWomen \n")
etable(mods_women_out, keep = "%group_treated:post$")
etable(mods_women_col, keep = "%group_treated:post$")

# Splitting the sample by gender yields little evidence of a significant policy 
# impact on the three individual outcomes for either men or women. 
# For men, the estimated post-treatment effect is negative for outcome_1 and 
# outcome_2 and positive for outcome_3, but none of these coefficients is statistically 
# significant. 
# For women, all three outcome coefficients are also statistically insignificant.

# In contrast, the collaboration results suggest that the policy mainly affects 
# within-group collaboration, especially for men. 
# Among men, the number of internal partners increases significantly (+0.566, p<0.05) 
# and internal collaboration volume rises strongly (+1.314, p<0.01), while external 
# collaboration volume decreases significantly (−0.910, p<0.05). This pattern is 
# consistent with a substitution toward internal collaborations. 
# For women, internal partnering also increases significantly (+0.474, p<0.05) and 
# internal collaboration volume rises (+0.869, p<0.05). However, external outcomes 
# are not statistically significant for women (external partners and external 
# collaboration volume). 
# The gender split suggests broadly similar qualitative patterns, stronger internal 
# collaboration post-policy, while the evidence for reduced external collaboration 
# is mainly driven by men.



##### a.2. Spillover effect ----
# Theoretically, in a network setting, individual outcomes do not depend only 
# on one’s own treatment status, but also on the treatment status of connected 
# peers. Standard DiD assumes no interference (SUTVA), meaning the treatment of 
# one unit does not affect others. However, network theory predicts spillovers: 
# funding received by one group can generate externalities through collaboration 
# links (knowledge diffusion, shared resources, reputation effects). Therefore, 
# outcomes should depend both on direct treatment and on exposure to treated 
# collaborators.

spill_by_subsample <- function(y, dt, expo_var){
  feols(as.formula(paste0(y, " ~ post:", expo_var, " | id + year")),
        data = dt[group_treated == 0],
        vcov = ~ group_id)
}

for (e in expo_list) {
  # Men (female == 0)
  mods_men_out_spill <- lapply(ind_outcomes, \(y) spill_by_subsample(y, df_sel_spill[female == 0], e))
  names(mods_men_out_spill) <- ind_outcomes
  mods_men_col_spill <- lapply(collab_vars, \(y) spill_by_subsample(y, df_sel_spill[female == 0], e))
  names(mods_men_col_spill) <- collab_vars
  # Women (female == 1)
  mods_women_out_spill <- lapply(ind_outcomes, \(y) spill_by_subsample(y, df_sel_spill[female == 1], e))
  names(mods_women_out_spill) <- ind_outcomes
  mods_women_col_spill <- lapply(collab_vars, \(y) spill_by_subsample(y, df_sel_spill[female == 1], e))
  names(mods_women_col_spill) <- collab_vars
  
  cat("\nMen (female=0) - Individual outcomes\n")
  print(etable(mods_men_out_spill, keep = paste0("%post:", e, "$")))
  cat("\nMen (female=0) - Collaboration outcomes\n")
  print(etable(mods_men_col_spill, keep = paste0("%post:", e, "$")))
  
  cat("\nWomen (female=1) - Individual outcomes\n")
  print(etable(mods_women_out_spill, keep = paste0("%post:", e, "$")))
  cat("\nWomen (female=1) - Collaboration outcomes\n")
  print(etable(mods_women_col_spill, keep = paste0("%post:", e, "$")))
}

# Spillovers are broader and more consistently negative for women: under the intensity 
# measure, each additional pre-policy treated partner is associated with significant 
# declines in all three individual outcomes and a significant contraction across 
# several collaboration dimensions (probability of collaborating, number of partners, 
# total volume, and both internal and external volumes). For men, significant spillovers 
# mainly affect collaboration patterns: internal collaboration declines and the probability 
# of collaborating falls, while external collaboration and the number of external partners 
# increase significantly, pointing to a reallocation from internal to external links 
# rather than a generalized decline. Overall, pre-policy exposure to treated collaborators 
# appears more adverse and more widespread for women, whereas for men the main adjustment 
# is a reshuffling of collaboration activity.



#### b) Heterogeneity by core --------------------------------------------------
##### b.1. Direct effect ----
# Core
mods_core_out <- lapply(ind_outcomes, \(y) did_by_subsample(y, df_sel[core == 1]))
names(mods_core_out) <- ind_outcomes
mods_core_col <- lapply(collab_vars, \(y) did_by_subsample(y, df_sel[core == 1]))
names(mods_core_col) <- collab_vars

# Non-core
mods_per_out <- lapply(ind_outcomes, \(y) did_by_subsample(y, df_sel[core == 0]))
names(mods_per_out) <- ind_outcomes
mods_per_col <- lapply(collab_vars, \(y) did_by_subsample(y, df_sel[core == 0]))
names(mods_per_col) <- collab_vars


cat("\nCore \n")
etable(mods_core_out, keep = "%group_treated:post$")
etable(mods_core_col, keep = "%group_treated:post$")

cat("\nNon-core \n")
etable(mods_per_out, keep = "%group_treated:post$")
etable(mods_per_col, keep = "%group_treated:post$")
# Splitting the sample by core status reveals markedly different patterns. 
# For core members, the policy effect on individual outcomes is positive : outcome_3 
# increases significantly (+1.80, p<0.05) and outcome_2 is also positive and marginally 
# significant (+44.3, p<0.10), while outcome_1 is positive but not statistically 
# significant. Collaboration effects for core members are strong : the probability 
# of collaborating rises marginally (+0.070, p<0.10), the number of partners increases 
# substantially (+2.40, p<0.05), and total collaboration volume increases (+3.46, p<0.10). 
# This expansion is largely internal—internal partners (+1.78, p<0.05) and internal 
# collaboration volume (+2.94, p<0.05) both increase significantly, while external 
# collaboration volume is not significant, and external partners are only marginally positive.
# For non-core members, individual outcomes show no statistically significant effects. 
# On collaborations, the main effect again concentrates on the internal margin : internal 
# partners increase (+0.315, p<0.05) and internal collaboration volume rises (+0.802, p<0.10). 
# In contrast, external collaboration volume decreases (−0.573, p<0.10), and external 
# partners show no significant change.
# The heterogeneity results suggest that the policy generates larger and broader 
# gains for core members, especially in partnering and internal collaboration, whereas 
# for non-core members the evidence points mainly to increased internal collaboration 
# and a possible substitution away from external collaboration volume



##### b.2. Spillover effect ----
for (e in expo_list) {
  # Core (core == 1)
  mods_core_out_spill <- lapply(ind_outcomes, \(y) spill_by_subsample(y, df_sel_spill[core == 1], e))
  names(mods_core_out_spill) <- ind_outcomes
  mods_core_col_spill <- lapply(collab_vars, \(y) spill_by_subsample(y, df_sel_spill[core == 1], e))
  names(mods_core_col_spill) <- collab_vars
  # Non-core (core == 0)
  mods_noncore_out_spill <- lapply(ind_outcomes, \(y) spill_by_subsample(y, df_sel_spill[core == 0], e))
  names(mods_noncore_out_spill) <- ind_outcomes
  mods_noncore_col_spill <- lapply(collab_vars, \(y) spill_by_subsample(y, df_sel_spill[core == 0], e))
  names(mods_noncore_col_spill) <- collab_vars
  
  cat("\nCore (core=1) - Individual outcomes\n")
  print(etable(mods_core_out_spill, keep = paste0("%post:", e, "$")))
  cat("\nCore (core=1) - Collaboration outcomes\n")
  print(etable(mods_core_col_spill, keep = paste0("%post:", e, "$")))
  
  cat("\nNon-core (core=0) - Individual outcomes\n")
  print(etable(mods_noncore_out_spill, keep = paste0("%post:", e, "$")))
  cat("\nNon-core (core=0) - Collaboration outcomes\n")
  print(etable(mods_noncore_col_spill, keep = paste0("%post:", e, "$")))
}
# Spillovers are stronger and more pervasive among core members. Under the intensity 
# exposure measure, each additional pre-policy treated partner is associated for core 
# individuals with significant declines in all three individual outcomes and significant 
# reductions across all collaboration indicators (probability of collaborating, number 
# of partners, total volume, and both internal and external volumes). 
# For non-core members, exposure intensity does not generate significant effects 
# on individual outcomes and the most robust effect is primarily a decline in internal 
# collaboration.
# Pre-policy exposure to treated collaborators appears to have much larger indirect 
# consequences for core members, whereas for non-core individuals the detectable 
# effects are more limited and concentrated on specific collaboration dimensions.






###############################################################################.
##  III- Impact of treatment on link formation  ------------------------
###############################################################################.
# In this section, we study the effect of the policy on the formation of collaboration links.
# The objective is to understand whether, after 2012, the policy changed the probability that a dyad is connected,
# that is, whether a collaboration link exists between two individuals in a given year.
# The analysis addresses straightforward questions. First, did the policy increase (or reduce) the frequency of links between individuals?
# Second, does the effect differ depending on whether only one side of the dyad is treated or both are treated?
# In other words, do we observe a specific impact for “treated–untreated” links (one treated) compared to “treated–treated” links (two treated),
# relative to dyads where neither individual is treated (the reference category)?
# Finally, we aim to assess the magnitude of the estimated effects by comparing them to the average frequency of a link in the data,
# in order to interpret the results in terms of order of magnitude.


df_work <- df[!is.na(group_treated_1) & !is.na(group_treated_2) &
                selection_1 == 1 & selection_2 == 1]



# We choose to do our analysis on df_work because it is the most relevant and the one 
# that will be utilised for our DiD analysis 
# The seelction criteria make us comapre inmdividuals that have the same criteria and the same
# probability to be chosen for the treatment which is better and makes moire sense to analyse. 



###############################################################################.
### III.1 Econometric Specification (Dyadic Difference-in-Differences) ------------------------------------------------------
###############################################################################.
# Joanne's part 

## Estimated model:
##
## y_{ij, g_i, g_j, t} =
##     α
##   + β  |T_{g_i} - T_{g_j}| × Post_t
##   + λ  (T_{g_i} × T_{g_j}) × Post_t
##   + γ' X_{ij,t}
##   + δ_t
##   + θ_{ij}
##   + ε_{ij, g_i, g_j, t}
##
## 
## Detailed interpretation of each component
## 

## 1) Dependent variable
## y_{ij,t} = 1 if dyad (i,j) has at least one collaboration link in year t,
##            0 otherwise.
##
## Therefore, we model the probability that a link exists between i and j
## at time t (a linear probability model with fixed effects).

## 

## 2) Treatment structure
##
## T_{g_i}, T_{g_j} ∈ {0,1} are treatment indicators
## for the groups to which individuals i and j belong.

## a) |T_{g_i} - T_{g_j}|
##
## = 1 if exactly one member of the dyad is treated
##     ("treated–untreated" dyad),
## = 0 otherwise.
##
## The coefficient β therefore measures the differential
## post-policy effect on mixed dyads (one treated),
## relative to the reference category (both untreated).

## b) (T_{g_i} × T_{g_j})
##
## = 1 if both individuals are treated
##     ("treated–treated" dyad),
## = 0 otherwise.
##
## The coefficient λ measures the differential post-policy effect
## on fully treated dyads,
## again relative to the reference group.

## 
## Reference category
##
## The omitted group (baseline) is:
## T_{g_i} = 0 and T_{g_j} = 0 → untreated dyads.
##
## All estimated effects (β and λ) are interpreted
## relative to this group.

## 

## 3) Post_t
##
## Post_t = 1 in the post-policy period (after 2012),
##         = 0 in the pre-policy period.
##
## Because treatment indicators are interacted with Post_t,
## β and λ are Difference-in-Differences (DiD) estimators.
##
## They capture how link formation changes after the policy
## for treated dyads relative to untreated dyads.

##

## 4) X_{ij,t}
##
## Vector of dyad-specific, time-varying controls:
## e.g., maximum age, age difference,
##       gender composition, core status, etc.
##
## The vector γ measures the marginal effect
## of these observable characteristics
## on the probability of link formation.

## 

## 5) θ_{ij} : Dyad fixed effects
##
## One fixed effect per dyad (i,j).
##
## These absorb all time-invariant unobserved heterogeneity
## specific to the pair:
##   - baseline affinity
##   - scientific proximity
##   - institutional overlap
##   - persistent collaboration culture
##   - any stable unobservable similarity
##
## Identification therefore comes from
## within-dyad variation over time.
##
## In other words, we compare each dyad
## to itself before and after the policy.
##
## The model does NOT compare different dyads to each other
## for identification.

##

## 6) δ_t : Year fixed effects
##
## These capture shocks common to all dyads in year t:
##   - global collaboration trends
##   - macro funding changes
##   - economic conditions
##   - network-wide expansion
##   - general time trends
##
## This ensures that β and λ do not capture
## overall time effects,
## but only differential changes for treated dyads.\
## Year fixed effects absorb any macro-level or network-wide variation affecting all 
## dyads equally in a given year, ensuring that identification comes only from differential 
## changes across dyads relative to the yearly average.

##
## 7) ε_{ij,t} : Error term
##
## Residual variation not explained by:
##   - treatment interactions
##   - controls
##   - dyad fixed effects
##   - year fixed effects
##
## Standard assumption:
## E[ε_{ij,t} | X, FE] = 0
##
## That is, conditional on controls and fixed effects,
## the remaining variation is mean-independent.



#library(data.table)
#library(fixest)
df[, y := as.integer(yearly_collabs_dyad > 0)]


df[, `:=`(
  one_treated  = as.integer(abs(group_treated_1 - group_treated_2)),  # |T_gi - T_gj|
  both_treated = as.integer(group_treated_1 * group_treated_2)        # T_gi*T_gj
)]


df_work <- df[!is.na(group_treated_1) & !is.na(group_treated_2) &
                selection_1 == 1 & selection_2 == 1]  #we select only the data with selection criteria 1 and 2 


X <- ~ agemax + agedif + female_1 + female_2 + core_1 + core_2
na_ctrl <- colSums(is.na(df_work[, .(agemax, agedif, female_1, female_2, core_1, core_2)]))
na_ctrl
## 
## Transition from the previous code:
## - Up to now, we defined:
##   (i) the outcome y_{ij,t} = 1{collab > 0},
##   (ii) dyadic treatment variables one_treated_ij and both_treated_ij,
##   (iii) a clean working sample (non-missing treatment status),
##   (iv) dyad-level controls X_{ij,t}.
##
## Next, we estimate the DiD model using dyad FE and year FE:
##   y ~ one_treated×post + both_treated×post + X + FE(dyad) + FE(year)
##
## The additional step below is NOT just estimation:
## it is also a robustness exercise.
## We re-estimate the same model under different selection rules
## (selection_1 only, selection_2 only, and both),
## to check whether results depend on how we define the "in-sample" dyads.

###############################################################################.
### III.2 Different selection --------------------------------------------------------------
###############################################################################.


## Model specification (conceptual reminder)
## 
## y_{ij,t} = α + β(one_treated_ij × post_t) + λ(both_treated_ij × post_t)
##            + γ'X_{ij,t} + θ_{ij} + δ_t + ε_{ij,t}
##
## - θ_{ij}: dyad fixed effects (controls for time-invariant pair heterogeneity)
## - δ_t   : year fixed effects (controls for common shocks / time trends)
## - cluster SE: two-way clustering by id_1 and id_2 (accounts for dependence
##               due to individuals appearing in multiple dyads)


## 
## A) Sample 1: keep dyads satisfying selection_1 for ego-side
## 

## Goal: estimate the model on the sub-sample defined by selection_1.
## This tests whether the estimated effects are sensitive to this inclusion rule.
df_s1 <- df[selection_1 == 1]
df_s1 <- df_s1[!is.na(group_treated_1) & !is.na(group_treated_2)]  # ensure treatment status observed for both sides

m_s1 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),     # θ_{ij} and δ_t
  cluster = ~ group_id_1 + group_id_2 ,            # two-way clustering 
  data    = df_s1
)



## --- Remove NA explicitly for all variables used in the model ---
vars_model <- c(
  "y", "one_treated", "both_treated", "post",
  "agemax", "agedif", "female_1", "female_2",
  "core_1", "core_2",
  "id_dyad", "year", "id_1", "id_2"
)

df_s1_clean <- df_s1[complete.cases(df_s1[, ..vars_model])]


m_s1 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s1_clean
)



m_s1 <- feols(
  y ~ one_treated:post + both_treated:post + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s1_clean
)

# pour voir si on garde core_2 ou pas 
df_s1_clean[, uniqueN(core_2), by = id_dyad][, summary(V1)]
# Très peu de dyades ont variation pas info supp.

m_s1 <- feols(
  y ~ one_treated:post + both_treated:post
  | id_dyad + year,
  cluster = ~ group_id_1 + group_id_2,
  data = df_s1_clean
)

etable(m_s1)

#After2012 the probability of collaboration increases by approximately 3.9 percentage points for fully treated dyads (*** significant at the 1% level), relative to untreated pairs.
#MixedDyads show a smaller increase of about 1.8 percentage points, but the effect is not statistically significant.

## 
## B) Sample 2: keep dyads satisfying selection_2 for partner-side
## 

## Same logic, but using the alternative selection rule selection_2.
df_s2 <- df[selection_2 == 1]
df_s2 <- df_s2[!is.na(group_treated_1) & !is.na(group_treated_2)]

m_s2 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s2
)



df_s2_clean <- df_s2[complete.cases(df_s2[, ..vars_model])]

m_s2 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s2_clean
)


m_s2 <- feols(
  y ~ one_treated:post + both_treated:post + core_1 ,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s2_clean
)

m_s2 <- feols(
  y ~ one_treated:post + both_treated:post ,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s2_clean
)


etable(m_s2)

#UnderSelection2 the probability of collaboration increases by about 3.7 percentage points for fully treated dyads (** significant at the 5% level) after 2012, relative to untreated pairs.

#MixedDyads also experience a 2.4 percentage point increase (* significant at the 10% level), suggesting a modest but statistically weaker spillover effect.



## 
## C) Sample 3: strict sample: selection_1 AND selection_2
## 

## This is the most restrictive sample:
## we keep only dyads where BOTH sides satisfy the selection criterion.
## Purpose: mimic a stricter "as-if experiment" sample and verify robustness.
df_s12 <- df[selection_1 == 1 & selection_2 == 1]
df_s12 <- df_s12[!is.na(group_treated_1) & !is.na(group_treated_2)]

m_s12 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s12
)

df_s12_clean <- df_s12[complete.cases(df_s12[, ..vars_model])]



m_s12 <- feols(
  y ~ one_treated:post + both_treated:post +
    agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s12_clean
)


m_s12 <- feols(
  y ~ one_treated:post + both_treated:post,
  fixef   = c("id_dyad", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data    = df_s12_clean
)

etable(m_s12)

# Under The Strictest Selection the probability of collaboration increases by about 7.0 percentage points for fully treated dyads (** significant at the 5% level) after 2012.
# MixedDyads show no statistically significant change, suggesting that the policy impact is concentrated primarily among pairs where both individuals are treated.


## 
## D) Reporting: compare coefficients across samples
## 

## etable displays the models side-by-side.
## We keep only the two DiD terms of interest:
## - one_treated:post  (β): mixed dyads after policy
## - both_treated:post (λ): treated-treated dyads after policy
etable(
  m_s1, m_s2, m_s12,
  headers = c("Selection 1", "Selection 2", "Selection 1 & 2"),
  keep    = c("%one_treated:post", "%post:both_treated")
)

## We choose m_s12 as our preferred specification because it relies on the
## most restrictive and economically coherent sample definition.
## By requiring that both members of the dyad satisfy the selection criterion,
## we ensure that comparisons are made within a well-defined and symmetric population,
## thereby limiting potential compositional bias.

## Although this restriction reduces the sample size, it strengthens identification
## by approximating a more credible “as-if experimental” framework.
## Importantly, the positive and statistically significant effect for fully treated dyads
## remains robust, and even increases in magnitude, under this strict selection rule,
## reinforcing the interpretation that the policy primarily strengthened collaboration
## within treated groups.


###############################################################################.
### III.3 visualize PRE-treatment trends (before 2012) by dyad type --------------------------------------------------------------
###############################################################################.


## 

## Why we do this:
## In a DiD design, the main threat is a violation of parallel trends:
## treated dyads may already follow a different trajectory BEFORE 2012.
## This plot checks whether the average link probability evolves similarly
## across groups prior to the policy.

## 
## 1) Build dyadic treatment indicators (defined only if both sides observed)
## 
## group_treated_1, group_treated_2 ∈ {0,1} indicate whether each side belongs
## to a treated group. We create:
## - one_treated  = |Tgi - Tgj|  → 1 if exactly one side is treated (mixed dyad)
## - both_treated = Tgi * Tgj    → 1 if both sides are treated (fully treated dyad)
df_work[!is.na(group_treated_1) & !is.na(group_treated_2),
        `:=`(
          one_treated  = as.integer(abs(group_treated_1 - group_treated_2)),  # |Tgi - Tgj|
          both_treated = as.integer(group_treated_1 * group_treated_2)        # Tgi*Tgj
        )
]

## 
## 2) Create a readable group label for plotting
## 
## Three mutually exclusive categories:
## - "Both treated"      : both_treated == 1
## - "Mixed (one treated)": one_treated == 1 (and both_treated == 0)
## - "None treated"      : baseline case (no one treated)
df_work[, group_type := fifelse(both_treated == 1, "Both treated",
                                fifelse(one_treated == 1, "Mixed (one treated)",
                                        "None treated"))]

## 
## 3) Restrict to PRE-policy period and keep only well-defined dyads
## 
## We only want years < 2012 (pre-treatment window), and we remove rows where
## treatment status is missing (otherwise group_type could be misleading).
df_pre <- df_work[year < 2012 & !is.na(one_treated) & !is.na(both_treated)]

## 
## 4) Aggregate: mean link probability by year and dyad group
## 
## mean_link = E[y_ijt] within each (year × group_type).
## Since y is a 0/1 indicator, mean(y) is a proportion:
## e.g., mean_link = 0.05 means 5% of dyads have a link that year.
df_plot <- df_pre[, .(mean_link = mean(y)), by = .(year, group_type)]
df_plot

## 
## 5) Plot: pre-treatment trends with clearly visible axes
##

## Increase margins so axis labels/ticks are not cut off
par(mar = c(5, 5, 4, 2))

## Create an empty plot (we will draw everything manually)
## axes = FALSE lets us fully control tick marks and labels.
plot(NA,
     xlim = range(df_plot$year),
     ylim = range(df_plot$mean_link),
     xlab = "Year",
     ylab = "Mean probability of collaboration",
     main = "Pre-treatment Trends (Before 2012)",
     axes = FALSE)

## Add axes manually:
## - x-axis: show each available year in the pre period
## - y-axis: las = 1 makes y-axis labels horizontal (more readable)
axis(1, at = unique(df_plot$year), labels = unique(df_plot$year), cex.axis = 1.1)
axis(2, las = 1, cex.axis = 1.1)

## Draw a box around the plotting region (cleaner look)
box()

##
## 6) Add the trend lines for each group
## 
## Colors correspond to the 3 dyad types.
## We loop over group_type, order points by year, then draw lines+points.
cols <- c("black", "orange", "blue")

i <- 1
for (g in unique(df_plot$group_type)) {
  tmp <- df_plot[group_type == g][order(year)]
  lines(tmp$year, tmp$mean_link,
        type = "b",   # both points and lines
        lwd  = 2,     # thicker lines for visibility
        pch  = 16,    # filled points
        col  = cols[i])
  i <- i + 1
}

## 
## 7) Mark the policy date (2012) as a visual reference
## 
## Even though the plot is pre-2012, this dashed vertical line reminds the reader
## where the policy starts.
abline(v = 2012, lty = 2, lwd = 2, col = "red")

## 
## 8) Add legend
## 
legend("topleft",
       legend = unique(df_plot$group_type),
       col = cols,
       lwd = 2,
       pch = 16,
       bty = "n")


## The graph displays the evolution of the mean probability of collaboration
## (E[y_ijt]) for different types of dyads between 2005 and 2011, that is,
## before the implementation of the policy. The x-axis represents time (year),
## while the y-axis represents the average probability that a collaboration link
## exists within a dyad in a given year.

## All three groups (both treated, mixed, and none treated) exhibit a gradual
## upward trend over the pre-treatment period. Although the levels differ
## across groups—mixed dyads having a lower baseline probability—the slopes
## appear broadly similar over time. Importantly, there is no visible divergence
## in trends before 2012, suggesting that treated and untreated dyads evolved
## in a comparable manner prior to the reform.

## This visual evidence supports the parallel trends assumption required for
## the validity of the Difference-in-Differences strategy, indicating that
## any post-2012 divergence can more credibly be attributed to the policy
## rather than to pre-existing differential dynamics.




###############################################################################.
### III.4 Heterogeneous Effects: Within-Group vs Across-Group Dyads --------------------------------------------------------------
###############################################################################.

## After estimating the overall policy effect on collaboration,
## we investigate whether the impact differs depending on whether
## the two individuals belong to the same group or to different groups.
## This allows us to distinguish between a strengthening of internal
## group cohesion (within-group links) and an increase in cross-group
## integration (across-group links).

## The variable `same_group` equals 1 when both individuals in the dyad
## belong to the same group, and 0 otherwise. We then estimate the same
## Difference-in-Differences specification separately for:
## (i) dyads within the same group (same_group == 1), and
## (ii) dyads across different groups (same_group == 0).

## In both regressions, we include dyad fixed effects and year fixed effects.
## Dyad fixed effects control for time-invariant characteristics specific
## to each pair (baseline affinity, structural proximity, persistent ties),
## while year fixed effects absorb common shocks affecting all dyads in a
## given year. Standard errors are clustered at the individual level
## (id_1 and id_2) to account for dependence across dyads sharing members.

## By comparing the coefficients on one_treated × post and both_treated × post
## across the two samples, we can determine whether the policy primarily
## reinforced collaboration within treated groups or whether it also
## stimulated collaboration across group boundaries.


df_work[, same_group := as.integer(group_id_1 == group_id_2)] 


#### within ####

m_within <- feols(
  y ~ one_treated:post + both_treated:post + agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef = c("id_dyad_in_group", "year"),
  cluster = ~  group_id_1 + group_id_2,
  data = df_work[same_group == 1]
)

vars <- c("y", "one_treated", "both_treated", "post",
          "id_dyad_in_group", "year", "id_1", "id_2")

df_within_clean <- df_work[
  same_group == 1 &
    complete.cases(df_work[, ..vars])
]

m_within <- feols(
  y ~ one_treated:post + post:both_treated
  | id_dyad_in_group + year,
  cluster = ~  group_id_1 + group_id_2,
  data = df_within_clean
)

## In the within-group sample (same_group == 1), both individuals
## in the dyad belong to the same group. This implies that they
## necessarily share the same treatment status:
##
## - If the group is treated → both individuals are treated.
## - If the group is not treated → neither individual is treated.
##
## Therefore, it is impossible to observe a “mixed” dyad
## (i.e., exactly one treated individual) within the same group.
## Formally, for all observations in this sample:
##
##     one_treated = |T_gi - T_gj| = 0
##
## As a result, the interaction term one_treated × post is equal to 0
## for all observations. Since this variable has no variation,
## it is perfectly collinear and automatically removed by fixest.
##
## This is not a problem, but a logical consequence of the sample
## restriction. In the within-group specification, only the effect
## for fully treated dyads (both_treated × post) can be identified.

m_within <- feols(
  y ~ post:both_treated
  | id_dyad_in_group + year,
  cluster = ~  group_id_1 + group_id_2,
  data = df_within_clean
)



#### outside ####

m_outside <- feols(
  y ~ one_treated:post + both_treated:post + agemax + agedif + female_1 + female_2 + core_1 + core_2,
  fixef = c("id_dyad_in_group", "year"),
  cluster = ~ group_id_1 + group_id_2,
  data = df_work[same_group == 0]
)

vars_out <- c("y", "one_treated", "both_treated", "post",
              "id_dyad_in_group", "year", "id_1", "id_2")

df_outside_clean <- df_work[
  same_group == 0 &
    complete.cases(df_work[, ..vars_out])
]

m_outside <- feols(
  y ~ one_treated:post
  | id_dyad_in_group + year,
  cluster = ~ group_id_1 + group_id_2,
  data = df_outside_clean
)


#### results ####

etable(m_within, m_outside)

## Within-group sample:
## The coefficient on post × both_treated is 0.0705 and statistically
## significant at the 10% level. This implies that, after 2012,
## the probability of collaboration between fully treated individuals
## within the same group increased by approximately 7 percentage points
## relative to untreated dyads. This suggests a strong internal
## reinforcement effect of the policy.

## Outside-group sample:
## The coefficient on one_treated × post is negative (-0.0287)
## and not statistically significant. This indicates that the policy
## did not produce a meaningful increase in collaboration across
## group boundaries. There is no evidence of spillover effects
## from treated to untreated individuals across groups.

## Overall conclusion:
## The policy effect appears to be concentrated within treated groups,
## strengthening internal collaboration rather than promoting
## cross-group integration.






###############################################################################.
### III.5 Heterogeneous Effects: Core vs Non Core &  Female vs Male--------------------------------------------------------------
###############################################################################.
## 
## Build dyad-type labels (core composition and gender composition)
## 

## core_pair classifies each dyad based on the "core" status of both members:
## - "core-core"         : both individuals are in the core (core_1=1 and core_2=1)
## - "noncore-noncore"   : neither individual is in the core (core_1=0 and core_2=0)
## - "core-noncore"      : mixed dyad (exactly one is core)
## This is useful to run heterogeneous DiD effects by dyad composition.
df_work[, core_pair := fifelse(core_1 == 1 & core_2 == 1, "core-core",
                               fifelse(core_1 == 0 & core_2 == 0, "noncore-noncore",
                                       "core-noncore"))]

## gender_pair classifies each dyad by gender composition:
## - "F-F"   : both are female
## - "M-M"   : both are male
## - "Mixed" : one female and one male
## This allows comparing policy effects across same-gender vs mixed-gender dyads.
df_work[, gender_pair := fifelse(female_1 == 1 & female_2 == 1, "F-F",
                                 fifelse(female_1 == 0 & female_2 == 0, "M-M",
                                         "Mixed"))]

## 
## Define controls and wrap the DiD estimation in a reusable function
## 

## ctrls stores the (time-varying) dyad-level controls included in the regression.
## Note: with dyad fixed effects, only controls that vary within dyad over time
## contribute to identification; time-invariant controls are absorbed by dyad FE.
ctrls <- "agemax + agedif"

## run_did(dt) estimates the DiD specification on any subset dt of the data.
## It keeps the model identical across subsamples (e.g., by core_pair or gender_pair),
## ensuring results are comparable.
##
## Model estimated:
## y_{ij,t} = β(one_treated_ij × post_t) + λ(both_treated_ij × post_t)
##            + γ'X_{ij,t} + θ_{ij} + δ_t + ε_{ij,t}
##
## - fixef = c("id_dyad","year") adds dyad FE (θ_ij) and year FE (δ_t)
## - cluster = c("id_1","id_2") implements two-way clustering at the individual level
##   to account for dependence across dyads that share a member.
run_did <- function(dt){
  feols(
    as.formula(paste("y ~ one_treated:post + both_treated:post +", ctrls)),
    fixef   = c("id_dyad", "year"),
    cluster = c("id_1", "id_2"),
    data    = dt
  )
}

#### Core vs Non Core ####

## 
## Heterogeneity analysis by (i) same-group vs cross-group dyads
## and (ii) core-composition of the pair (core_pair)
##

## We estimate the same DiD specification on increasingly granular subsamples.
## The objective is to test whether the post-policy effect differs depending on:
##  - whether the collaboration is within the same group (same_group == 1)
##    or across different groups (same_group == 0),
##  - and whether the dyad is core-core, core-noncore, or noncore-noncore.
##
## This helps interpret the mechanism:
## e.g., does the policy primarily strengthen ties among core members,
## mixed-status pairs, or noncore members?

## 
## A) WITHIN-GROUP dyads (same_group == 1)
## 
## In within-group dyads, both individuals share the same group, so they share
## the same group-level treatment status. As a consequence, “mixed-treatment”
## dyads (one treated) are typically impossible by construction:
##   one_treated = |T_gi - T_gj| = 0 for all within-group pairs.
## Therefore, one_treated × post often has no variation and may be dropped
## as collinear. The key estimable effect within groups is usually
## both_treated × post (fully treated pairs after 2012).

m_within_corecc  <- run_did(df_work[same_group == 1 & core_pair == "core-core"])
m_within_coremix <- run_did(df_work[same_group == 1 & core_pair == "core-noncore"])
m_within_noncc   <- run_did(df_work[same_group == 1 & core_pair == "noncore-noncore"])

## Side-by-side comparison for within-group subsamples.
## We keep only the DiD coefficients of interest:
##  - one_treated:post  (β): mixed dyads after policy (often not identified within-group)
##  - both_treated:post (λ): fully treated dyads after policy (main within-group effect)
etable(m_within_corecc, m_within_coremix, m_within_noncc,
       headers = c("Within: core-core", "Within: core-noncore", "Within: noncore-noncore"),
       keep = c("%one_treated:post", "%post:both_treated"))

## Within-group heterogeneity by core composition:

## Core–core dyads:
## The coefficient on post × both_treated is 0.2199 and highly significant (p < 0.001).
## This implies that after 2012, the probability of collaboration between two
## treated core members increased by approximately 22 percentage points.
## The policy effect is therefore very strong among central actors.

## Core–noncore dyads:
## The effect remains positive and statistically significant (0.1289, p < 0.001),
## corresponding to an increase of about 13 percentage points.
## This indicates that the reform also strengthened links between core and
## peripheral members, although less strongly than within the core itself.

## Noncore–noncore dyads:
## The coefficient (0.0223) is small and not statistically significant.
## This suggests that the policy did not meaningfully affect collaboration
## among peripheral members within treated groups.

## Overall conclusion:
## The policy primarily reinforced collaboration within the core of treated groups,
## with weaker effects for mixed-status pairs and no detectable impact among
## noncore–noncore dyads.


## 
## Diagnostic: why is one_treated × post missing in within-group models?
## 
## These counts verify whether one_treated varies in the within-group subsamples.
## If one_treated is always 0, then one_treated × post is always 0 as well,
## implying perfect collinearity → the regressor is dropped by fixest.

df_work[same_group == 1 & core_pair == "core-core",
        .N, by = .(one_treated, post)][order(one_treated, post)]

df_work[same_group == 1 & core_pair == "core-noncore",
        .N, by = .(one_treated, post)][order(one_treated, post)]

df_work[same_group == 1 & core_pair == "noncore-noncore",
        .N, by = .(one_treated, post)][order(one_treated, post)]


## 
## B) OUTSIDE-GROUP dyads (same_group == 0)
## 
## In cross-group dyads, individuals belong to different groups.
## If treatment is assigned at the group level, then fully treated dyads
## (both_treated = 1) may be rare or even impossible depending on the design.
## Conversely, mixed-treatment dyads (one_treated = 1) are typically the
## relevant comparison group for cross-group links.
##
## Estimating the DiD model separately here tests whether the policy generates
## spillovers across group boundaries (treated–untreated links across groups),
## and whether this differs by core composition of the dyad.

m_out_corecc  <- run_did(df_work[same_group == 0 & core_pair == "core-core"])
m_out_coremix <- run_did(df_work[same_group == 0 & core_pair == "core-noncore"])
m_out_noncc   <- run_did(df_work[same_group == 0 & core_pair == "noncore-noncore"])

## Side-by-side comparison for outside-group subsamples.
etable(m_out_corecc, m_out_coremix, m_out_noncc,
       headers = c("Outside: core-core", "Outside: core-noncore", "Outside: noncore-noncore"),
       keep = c("%one_treated:post", "%post:both_treated"))

## 
## Outside-group heterogeneity by core composition
## 
## Core–core dyads (across groups):
## The coefficient on one_treated × post is negative (-0.0850) and not
## statistically significant. This suggests that the policy did not
## increase cross-group collaboration among core members. If anything,
## the point estimate hints at a possible decline, but the effect is
## imprecisely estimated given the small sample size.

## Core–noncore dyads (across groups):
## Neither one_treated × post nor both_treated × post is statistically
## significant. This indicates no clear evidence of spillover effects
## across group boundaries for mixed-status pairs.

## Noncore–noncore dyads (across groups):
## Both coefficients are positive and statistically significant at the 5% level.
## The magnitudes (≈ 0.14–0.15) imply increases of roughly 14–15 percentage
## points in collaboration probability after 2012.
## This suggests that cross-group collaboration gains are concentrated
## among peripheral (noncore) actors.

## Overall conclusion:
## Unlike the strong within-core reinforcement observed earlier,
## cross-group effects appear to be driven primarily by noncore members,
## with little evidence of increased cross-group collaboration among core actors.



#### Female/Male ####

## 
## Heterogeneity analysis by gender composition of the dyad
## 

## We now investigate whether the policy effect differs depending
## on the gender composition of the pair (F-F, M-M, Mixed).
## The same DiD specification is estimated separately for:
##  - within-group dyads (same_group == 1)
##  - across-group dyads (same_group == 0)
##
## This allows us to test whether the reform reinforced collaboration
## differently for female-only, male-only, or mixed-gender pairs.

## 
## A) WITHIN-GROUP dyads (same_group == 1)
## 

m_within_FF    <- run_did(df_work[same_group == 1 & gender_pair == "F-F"])
m_within_MM    <- run_did(df_work[same_group == 1 & gender_pair == "M-M"])
m_within_mixed <- run_did(df_work[same_group == 1 & gender_pair == "Mixed"])

## The table compares DiD coefficients across gender compositions.
## In within-group subsamples, one_treated × post is typically not identified,
## because treatment is assigned at the group level.
## Within the same group, both individuals necessarily share
## the same treatment status (either both treated or both untreated).
## Hence, mixed-treatment dyads do not exist in this sample.

etable(m_within_FF, m_within_MM, m_within_mixed,
       headers = c("Within: F-F", "Within: M-M", "Within: Mixed"),
       keep = c("%one_treated:post", "%post:both_treated"))
## 
## Within-group heterogeneity by gender composition
## 
## Female–female (F-F) dyads:
## The coefficient on post × both_treated is 0.0728, implying an increase
## of roughly 7 percentage points in collaboration probability after 2012.
## However, the estimate is not statistically significant, likely due to
## the smaller sample size in this subgroup.

## Male–male (M-M) dyads:
## The estimated effect is 0.0700 and statistically significant at the 5% level.
## This suggests that treated male pairs within the same group experienced
## a meaningful increase in collaboration following the policy.

## Mixed-gender dyads:
## The coefficient is 0.0638 and statistically significant at the 5% level,
## indicating a similar increase in collaboration probability (around 6–7
## percentage points) for mixed pairs.

## Overall conclusion:
## The magnitudes are very similar across gender compositions, suggesting
## that within groups the policy broadly strengthened collaboration,
## without disproportionately favoring a specific gender pairing.
## more the M-M and M-mixed ties 




## Diagnostic: verify absence of mixed-treatment variation
## If one_treated == 0 for all observations, the interaction term
## one_treated × post has no variation and is dropped due to collinearity.

df_work[same_group == 1 & gender_pair == "F-F",
        .N, by = .(one_treated, post)][order(one_treated, post)]

df_work[same_group == 1 & gender_pair == "M-M",
        .N, by = .(one_treated, post)][order(one_treated, post)]

df_work[same_group == 1 & gender_pair == "Mixed",
        .N, by = .(one_treated, post)][order(one_treated, post)]


## 
## B) OUTSIDE-GROUP dyads (same_group == 0)
##

m_out_FF    <- run_did(df_work[same_group == 0 & gender_pair == "F-F"])
m_out_MM    <- run_did(df_work[same_group == 0 & gender_pair == "M-M"])
m_out_mixed <- run_did(df_work[same_group == 0 & gender_pair == "Mixed"])

## In cross-group dyads, mixed-treatment configurations are possible,
## since individuals belong to different groups and may have different
## treatment statuses. Therefore, one_treated × post can be identified.
##
## Comparing coefficients across F-F, M-M, and Mixed dyads allows us
## to assess whether the policy generated gender-specific spillovers
## across group boundaries.

etable(m_out_FF, m_out_MM, m_out_mixed,
       headers = c("Outside: F-F", "Outside: M-M", "Outside: Mixed"),
       keep = c("%one_treated:post", "%post:both_treated"))


## 
## Outside-group heterogeneity by gender composition
## 

## Female–female (F-F) dyads:
## The coefficient on one_treated × post is 0.1121 and statistically
## significant at the 5% level, implying an increase of roughly
## 11 percentage points in cross-group collaboration when exactly
## one member is treated. In addition, post × both_treated is positive
## (0.1050) and significant at the 10% level, suggesting that fully
## treated female pairs across groups also experienced an increase
## in collaboration after 2012.

## Male–male (M-M) dyads:
## The coefficient on one_treated × post is positive (0.0460) but not
## statistically significant. However, post × both_treated is positive
## (0.1195) and significant at the 5% level, indicating that fully
## treated male dyads across groups saw an increase of about 12
## percentage points in collaboration probability.

## Mixed-gender dyads:
## Both coefficients are positive (≈ 0.106 and 0.119), but neither
## is statistically significant. While the magnitudes are comparable
## to those observed for single-gender dyads, the estimates are
## imprecise, likely due to smaller effective variation in this subgroup.

## Overall conclusion:
## Outside group boundaries, the policy appears to have increased
## collaboration particularly among same-gender dyads (especially
## female–female and fully treated male–male pairs), while effects
## for mixed-gender dyads are less precisely estimated. This suggests
## that cross-group spillovers may have operated more strongly within
## gender-homogeneous networks.
