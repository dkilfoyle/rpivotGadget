tmplTableCount =
'# grouped by = {{groupby}}
{{#spread}}
{{#group3}}ftable({{/group3}}addmargins(xtabs(~ {{groupbyPlus}}, {{df}})){{#group3}}){{/group3}}
{{/spread}}{{^spread}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n())
{{/spread}}'

tmplTableAgg =
'# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise({{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) {{#spread}}{{#group2}}%>%
  spread({{group2}}, {{vals}}.{{agg}}){{/group2}}{{/spread}}'

tmplBarCount =
'# grouped by {{groupby}}
ggplot({{df}}, aes(x=as.factor({{group1}}){{#group2}}, fill=as.factor({{group2}}){{/group2}})) + {{#group3}}
  facet_grid(. ~{{group3}}) + {{/group3}}
  geom_bar({{#bar}}position="dodge"{{/bar}})'

tmplBarAgg =
'# grouped by  = {{groupby}}
# aggregated to = {{vals}}
{{df}} %>%
  group_by({{groupby}}) %>%
  summarise(n=n(), {{vals}}.{{agg}}={{agg}}({{vals}}, na.rm=T)) %>%
    ggplot(aes(x=as.factor({{group1}}), y={{vals}}.{{agg}}{{#group2}}, fill=as.factor({{group2}}){{/group2}})) +{{#group3}}
      facet_grid(. ~{{group3}}) + {{/group3}}
      geom_bar(stat="identity"{{#bar}}, position="dodge"{{/bar}})'
