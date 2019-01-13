 function [structure] = AnalyseSwitchingIE(log, sdp)
%
% clear all; close all
% sdp = 3;
% fileID = fopen('LSw_IE6.txt');
% log = textscan(fileID,'%s %f %d %s');
% fclose(fileID);

% this function summaryses responses to the language switching task.
% It takes a single log, previously edited with coding for incorrect responses
% it excludes incorrect resposnes and responses following an incorrect
% it calculates the mean RTs by category, excluding outliers
% it returns a structure containing the processed log, a table containing
% percentage of excluded and correct trials, mean RTS and sd, and an array
% (CheckParity) that checks the integrity of the experimental script
% (so it has to be [64, 64, 64, 64, 64, 64])


%% use the label of the wav file to create a table

exclude = zeros(length(log{1,2}),1);
for l = 1:length(log{1})
    
    tag{l,1} = log{1}{l};
    labels(l,:) = strsplit(char(tag(l)), '_');
    type(l,1) = labels(l, 2);
    lang(l,1) = labels(l,3);
    picture(l,1) = labels(l,4);
    ntrial(l,1) = str2double(cellstr(strrep(labels(l,5), '.wav','')));
    correct(l,1) = (log{3}(l));
    typeresp(l,1) = log{4}{l};
    
end

log = table(type, lang, picture, ntrial, log{1,2}(:), correct, typeresp);
log.Properties.VariableNames = {...
                'type'...
                'language'...
                'picture'...
                'ntrial'...
                'RT'...
                'correct'...
                'typeresp'};

% sort the table by trial order
[logc,index] = sortrows(log,{'ntrial'},{'ascend'}); 

% % Exclude 
% W = no answer or wrong language: 1+1
% H = hesitation or incomplete answer: 1
% C = corrected answer: 1
% N = ambient noise: 1


for l = 1:size(logc,1)
    if  table2array(logc(l,'typeresp')) == 'W'
        exclude(l,1) = 1;
    elseif  l >1 && table2array(logc(l-1,'typeresp')) == 'W'
        exclude(l,1) = 1;
    elseif table2array(logc(l,'typeresp')) == 'H' || table2array(logc(l,'typeresp')) == 'C'
        exclude(l,1) = 1;
    elseif table2array(logc(l,'typeresp')) == 'N'
        exclude(l,1) = 1;
        
    end
end
logc = [logc table(exclude)];


%% create index to find languages

iEng = strcmp(table2cell(logc(:,'language')), 'inglese');
Eng = logc(iEng, :);
iIta = strcmp(table2cell(logc(:,'language')), 'italiano');
Ita = logc(iIta, :);

% then divide by trial type
iblockedEng = strcmp(table2cell(logc(iEng, 'type')), 'blocked'); 
t.blockedEng = Eng(iblockedEng, :);
iblockedIta = strcmp(table2cell(logc(iIta, 'type')), 'blocked'); 
t.blockedIta = Ita(iblockedIta, :);

irepeatEng = strcmp(table2cell(logc(iEng, 'type')), 'repeat'); 
t.repeatEng = Eng(irepeatEng, :);
irepeatIta = strcmp(table2cell(logc(iIta, 'type')), 'repeat'); 
t.repeatIta = Ita(irepeatIta, :);

iswitchEng = strcmp(table2cell(logc(iEng, 'type')), 'switch'); 
t.switchEng = Eng(iswitchEng, :);
iswitchIta = strcmp(table2cell(logc(iIta, 'type')), 'switch'); 
t.switchIta = Ita(iswitchIta, :);

% then check they're all the same number :P
checkParity = [size(t.blockedEng,1); size(t.blockedIta,1);...
               size(t.repeatEng,1); size(t.repeatIta,1);...
               size(t.switchEng,1); size(t.switchIta,1)];
            

%% calculate the mean RT by category. exclude outliers by category
trialtypes = fieldnames(t);
aggregate = array2table(zeros(1,9));
aggregate.Properties.VariableNames = {'type','language','picture','ntrial',...
    'RT','correct','typeresp','exclude','validRT'};

for tt = 1: length(trialtypes)
    type = t.(trialtypes{tt});
    
    % mean by category
    for i = 1:size(type, 1)
        iincluded(i) = table2array(type(i,'exclude')) == 0;
        correctRT = table2array(type(iincluded,'RT'));
    end
    meanRT(tt,1) = mean(correctRT);
    sdRT(tt,1) = std(correctRT);
    
    % exclude outliers
    for ci = 1:size(type, 1)
        ivalid(ci,1) = table2array(type(ci, 'exclude')) == 0 &&...
            table2array(type(ci, 'RT')) < meanRT(tt,1)+(sdp*sdRT(tt,1)) &&...
            table2array(type(ci, 'RT')) > meanRT(tt,1)-(sdp*sdRT(tt,1));
        if ivalid(ci) == 0
            validRT(ci,1) = NaN;
        else
            validRT(ci,1) = table2array(type(ci, 'RT'));
        end
    end
   
    % recalculate the mean and sd
    t.(trialtypes{tt}) = [type table(validRT)];
    meanVRT(tt,1) = nanmean(validRT);
    sdVRT(tt,1) = nanstd(validRT);
    
    % count % outliers
    outliers(tt) = sum(iincluded) - sum(ivalid);
    
    % save processed log as a concatenated table
    aggregate = vertcat(aggregate, t.(trialtypes{tt}));
    
end

aggregate = aggregate(2:end, :);

%% calculate Mix and Switch costs for each language
    % Mix Cost = repeat - blocked; Switch Cost = switch - repeat

    mixEng = meanVRT(3, 1) - meanVRT(1, 1);
    mixIta = meanVRT(4, 1) - meanVRT(2, 1);
    switchEng = meanVRT(5, 1)- meanVRT(3, 1);
    switchIta = meanVRT(6, 1) - meanVRT(4, 1);
    
    
%% Count errors, excluded and outliers

% total
PerTotalCorrect = sum(correct)/length(correct)*100;
PerTotalOutliers = sum(outliers)*100/(64*6);
PerTotalExcluded = PerTotalOutliers + sum(exclude)/length(exclude)*100;

% per category
for tt = 1: length(trialtypes)
    type = t.(trialtypes{tt});
    TypeCorrect(1,tt) = sum(table2array(type(:, 'correct')))/64*100;
    TypeOutliers(1,tt) = outliers(tt)/64*100;
    TypeExcluded(1,tt) = TypeOutliers(1,tt) + sum(table2array(type(:, 'exclude'))/64*100);
end

% save them in a table
Excluded = array2table([PerTotalCorrect TypeCorrect; ...
            PerTotalOutliers TypeOutliers; ...
            PerTotalExcluded TypeExcluded;]);
Excluded.Properties.VariableNames = {...
                'total' 'blockedEng' 'blockedIta' 'repeatEng' 'repeatIta' 'switchEng' 'switchIta'};
Excluded.Properties.RowNames = {...
                'Perc.Correct' 'Outliers' 'Perc.Excluded'};


%% save relevant info in a structure to output

structure.processedLog = aggregate;
structure.Excluded = Excluded;
structure.meanVRT = meanVRT;
structure.sdVRT = sdVRT;
structure.checkParity = checkParity;
structure.costs = [mixEng mixIta switchEng switchIta];
 
 end
