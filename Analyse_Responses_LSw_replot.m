% Analyse responses and RTs of the Language Switching task
clear all; close all; clc


%% %%% parameters
done    = 0; % if data have already been read and summarized just read the data mat
plotind = 0; % plot each person if 1
compile = 1; % compile data in a table for presentation if 1

%%

if done == 0
    
group   = 'Ita-Eng';
sdp     = 3; % number of standard deviations to include RTs (3: Costa&Santesteban 2004,
             % Costa&Santesteban&Ivanova 2006, Calabria et al 2012, Macizo et al 2012;
             % 2.5: Ma et al 2016)


%% read the txt file containing RTs and errors
files = dir('LSw_IE*.txt');
files = sort_nat({files.name});

r = 1;
Excluded = zeros(3,7);
for s = 1:length(files)
    
    subjnum{s} = strrep(strrep(char(files(s)), '.txt', ''), 'LSw_', '');
    fileID = fopen(char(files(s)));
    dmdxlog = textscan(fileID,'%s %f %d %s');
    fclose(fileID);
    
    % analyse RTs calling the function
    data.(subjnum{s}) = AnalyseSwitchingIE(dmdxlog, sdp);
    
    % pool errors
    Correct(s, :) = table2array(data.(subjnum{s}).Excluded(1, :));
    
    % count excluded data
    Excluded = (Excluded+table2array(data.(subjnum{s}).Excluded));
    
    % pool RTs
    MeanRTs(s, :) = data.(subjnum{s}).meanVRT;
    
    % pool costs
    costs(s, :) = data.(subjnum{s}).costs.';
    
    dataset(r:r+383, :) = [cell2table(repmat(subjnum(s), 384, 1))...
        data.(subjnum{s}).processedLog];
    r = r+384;
       
    
    %% individual plots

    if plotind == 1
        
    figsubj = figure;
    plot([mean(MeanRTs(s,1)) mean(MeanRTs(s,3)) mean(MeanRTs(s,5))],...
        '- o','MarkerSize',5, 'LineWidth', 2)
    hold on
    plot([mean(MeanRTs(s,2)) mean(MeanRTs(s,4)) mean(MeanRTs(s,6))],...
        '- o','MarkerSize',5, 'LineWidth', 2)
    xlim([.5 3.5])
    ylim([400 1250])
    set(gca, 'XTick', 1:3)
    set(gca,'XTickLabel',{'Blocked' 'Repeat' 'Switch'});
    ylabel('ms')
    xlabel('Trial Type')
    legend('English','Italian', 'location', 'northwest')
    suptitle(['Compared RTs ' subjnum{s}])
    hold off
    saveas(figsubj, ['RTs_' subjnum{s}], 'tif')
    
    end

end

%%
if length(subjnum) > 1
    
%% print datasets to file

    
% errors & excluded data
Excluded = Excluded./s;
tExcluded = table(Excluded(:,1), Excluded(:,2), Excluded(:,3),...
    Excluded(:,4), Excluded(:,5), Excluded(:,6), Excluded(:,7));
tExcluded.Properties.VariableNames = {...
                 'total' 'blockedEng' 'blockedIta' 'repeatEng' 'repeatIta' 'switchEng' 'switchIta'};
tExcluded.Properties.RowNames = {...
                'Perc.Correct' 'Outliers' 'Perc.Excluded'};
writetable(tExcluded, 'ExcludedData_IE.csv', 'WriteRowNames',true);


% RTs
valid = ~isnan(table2array(dataset(:, 'validRT')));
datasetIE = dataset(valid, 1:6);
writetable(datasetIE, 'LSw_data_IE.csv');

% costs
subj = repmat(subjnum.', 4, 1);
lang = repmat([repmat({'eng'}, s, 1); repmat({'ita'}, s, 1)], 2, 1);
type = [repmat({'mix'}, s*2, 1); repmat({'switch'}, s*2, 1)];
costsp = reshape(costs, s*4, 1);
costsIE = table(subj, lang, type, costsp);
costsIE.Properties.VariableNames = {...
    'subject', ...
    'language', ...
    'type', ...
    'cost'};
writetable(costsIE, 'LSw_costs_IE.csv');

% add covariates - language history questionnaire variables
costsvar = addvar('LSw_data_IE.csv', 'all_variables_Ita-Eng_input.csv');
rtvar = addvar('LSw_data_IE.csv', 'all_variables_Ita-Eng_input.csv');


    
%% plot across participants
    
% percentage errors
figa = figure;
bar(mean(Correct(:,2:end), 1))
ylim([0 100])
hold on
set(gca,'XTickLabel',{'blockedEng' 'blockedIta' 'repeatEng' 'repeatIta' 'switchEng' 'switchIta'});
ylabel('% correct');
title(['Percentage of Correct trials. Tot. Correct:' sprintf('%.2f', mean(Correct(:,1))) '%'])
saveas (figa, ['errors_' group], 'tif')


% mean RTs relevant to mix and switch costs
figb = figure;

sqrtN = sqrt(length(subjnum));
    errorbar([mean(MeanRTs(:,1)) mean(MeanRTs(:,3)) mean(MeanRTs(:,5))],...
             [std(MeanRTs(:,1))..../sqrtN 
             std(MeanRTs(:,3))..../sqrtN
             std(MeanRTs(:,5))..../sqrtN
             ],...
             'LineWidth', 4, 'LineStyle', '-', 'Color', 'k')
    hold on
    errorbar([mean(MeanRTs(:,2)) mean(MeanRTs(:,4)) mean(MeanRTs(:,6))],...
             [std(MeanRTs(:,2))..../sqrtN
             std(MeanRTs(:,4))..../sqrtN
             std(MeanRTs(:,6))..../sqrtN
             ],...
             'LineWidth', 4, 'LineStyle', '-', 'Color', [.5 .5 .5])
    ylim([700 1150])
    xlim([.5 3.5])
    set(gca, 'XTick', 1:3)
    set(gca,'XTickLabel',{'Blocked' 'Repeat' 'Switch'});
    ylabel('ms')
    xlabel('Trial type')
hold on
legend('English','Italian', 'Location', 'southeast')
legend('boxoff')
suptitle(['RT. Italian-English. N = ' num2str(length(subjnum))])
hold off
saveas(figb, ['RTs_' group], 'tif')

% costs
figcosts = figure;
h = boxplot(costs);
set(h,'LineWidth', 1.5)
ylim([-80 280])
set(gca,'XTickLabel',{'MixEng' 'MixIta' 'SwitchEng' 'SwitchIta'});
ylabel('difference in RT')
xlabel('Mix Cost = repeat - blocked; Switch Cost = switch - repeat')
hold on
plot(xlim,[0 0],'color', [.5 .5 .5], 'LineStyle', ':')
title('Mix and Switch costs. Italian-English')
saveas(figcosts, ['costs_' group], 'tif')

% % costs as dots
% figcostsdots = figure;
% scatter(costs(:,1), 1:length(subjnum), 25, [0 1 0], 'filled')


% costs: violin plots!
figviolin = figure;
violin(costs,'facecolor',[1 1 1; 1 1 1; 1 1 1; 1 1 1],'edgecolor','k',...
'mc','',...
'medc','m');
legend off
ylim([-150 400])
set(gca, 'XTick', 1:4)
set(gca,'XTickLabel',{'MixEng' 'MixIta' 'SwitchEng' 'SwitchIta'});
ylabel('difference in RTs')
xlabel('Mix Cost = repeat - blocked; Switch Cost = switch - repeat')
hold on
plot(xlim,[0 0],'color', [.5 .5 .5], 'LineStyle', ':')
title('Mix and Switch costs in English and in Italian')
saveas(figviolin, ['costs_violin_' group], 'tif')


% save all the data in a mat. not optimal at all but good for accidents
when = datestr(now, 'dd_mm_yy_HHMM');
save(['dataIE_' when])

end

else
    display('load data manually')
    pause
end

%% compile a table to present data

if compile == 1
    
    tblocked = array2table([mean(MeanRTs(:,1)) std(MeanRTs(:,1)) ...
        mean(MeanRTs(:,2)) std(MeanRTs(:,2)) ]);
    trepeat = array2table([mean(MeanRTs(:,3)) std(MeanRTs(:,3)) ...
        mean(MeanRTs(:,4)) std(MeanRTs(:,4)) ]);
    tswitch = array2table([mean(MeanRTs(:,5)) std(MeanRTs(:,5)) ...
        mean(MeanRTs(:,6)) std(MeanRTs(:,6)) ]);
    tcmix = array2table([mean(costs(:, 1)) std(costs(:, 1))...
        mean(costs(:, 2)) std(costs(:, 2))]);
    tcswitch = array2table([mean(costs(:, 3)) std(costs(:, 3))...
        mean(costs(:, 4)) std(costs(:, 4))]);
    
    tpres = [tblocked; trepeat; tswitch; tcmix; tcswitch];
    tpres.Properties.VariableNames = {...
        'Eng_mean' 'Eng_stdev' 'Ita_mean' 'Ita_stdev' };
    tpres.Properties.RowNames = {...
        'blocked' 'repeat' 'switch' 'mix.cost' 'switch.cost'};
    writetable(tpres, ['SumRTs_IE.csv'], 'WriteRowNames',true)

end


