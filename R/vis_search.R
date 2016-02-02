

vis_search_subj <- vis_search %>% 
  unite(Distractor_Target, Distractor.type,Target) %>% 
  group_by(Distractor_Target,Number.of.distractors,randGator) %>% 
  tally(mean(RT..ms.))

ggplot(vis_search_subj, aes(x=Number.of.distractors, y=n)) +
  geom_line(size=1.5, aes(color = factor(Distractor_Target))) +
  geom_point(size=4, aes(shape = factor(Distractor_Target),color = Distractor_Target)) +
  facet_wrap(~randGator) + theme_minimal()

vis_search_grp <- vis_search_subj %>% 
  group_by(Distractor_Target, Number.of.distractors) %>% 
  tally(mean(n))

ggplot(vis_search_grp,aes(x=Number.of.distractors, y=n)) +
  geom_line(size=1.5, aes(color = Distractor_Target)) +
  geom_point(size = 4, aes(shape = factor(Distractor_Target),color = Distractor_Target)) +
  scale_y_continuous("RT (ms)") + theme_minimal()
