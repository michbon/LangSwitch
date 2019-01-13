 function [datavar] = addvar (dat, var)

% % % DEFINITION
% This function adds covariates, such as variables from the Language
% History Questionnaire, to datasets of responses which may have several
% rows for each subject
%
% the table containing covariates needs to be edited beforehand, to make
% sure the input to this function is ready to go to the stats.
%
% NB: in both 'data' and 'variables' the subject number array needs to be
% the first column
%
% % % INPUT
%  data       = the original dataset containing responses to an experiment
%  variables  = the table containing the variables of interest
%  group      = the name of your group, as a label for the output file name
%
% % % OUTPUT
% datavar = a dataset.csv

% clear all; clc
% var = 'all_variables_Ita-Eng_input.csv';
% dat = 'LSw_costs_IE.csv';

%

variables = readtable(var, 'ReadVariableNames', true);
vsubject = table2cell(variables(:, 1));

data = readtable(dat);
dsubject = table2cell(data(:, 1));


for d = 1:length(dsubject)
    for v = 1: length(vsubject)
        if strcmp(dsubject(d), vsubject(v))
            datavar(d, :) = [data(d, :) variables(v, 2:end)] ;
        end
    end  
end

writetable(datavar, [ strrep(dat, '.csv', '') '_var.csv'])

 end % of function


