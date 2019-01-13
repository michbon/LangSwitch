% Automatically define voice onset in an audio file by calculating the increase in amplitude.
% Plays the audio file and lets you save if the onset is valid
% and if the asnwer is correct(defined for language switching task)
%clear all; clc; close all;
set(0,'DefaultFigureWindowStyle','docked') % docks the figures to avoid a mess with windows


audiofile = dir('s14_practice*.wav');

for i = 1:length(audiofile)
    
    reading         = audioread(audiofile(i).name);
    info            = audioinfo(audiofile(i).name);
    aname           = strrep(cellstr(audiofile(i).name), '.wav', '');
    
    range           = [min(reading) max(reading)];  % a range of frequency values of audio signal
    threshold       = 2.1;            % a multiplier of baseline signal to define the threshold
    SilenceWindow   = 70;           % initial ms to define silence
    
    
    SilenceSamples  = info.SampleRate * SilenceWindow *.001;        % define silence
    SilenceValues   = [min(reading(1:SilenceSamples)) max(reading(1:SilenceSamples))];
    SilenceRange    = SilenceValues*threshold;      % multiply it n times to define a threshold for voice onset. n = threshold
    
    above           = reading <SilenceRange(1) & reading<SilenceRange(2);   % find where the values are above the threshold
    tier            = 1:length(reading);        % find the first of this points and calculate where in time it is
    time            = (tier-1)/info.SampleRate; % extract time to use in plot
    
    if sum(above) == 0      % make the script robust to lack of onset and records it
        OnsetPoint  = max(tier);
        hasOnset    = 0;
    else
        OnsetPoint  = min(tier(above))-1;
        hasOnset    = 1;
    end    
    
    fig = figure;           % plot the audio with the Onset
        OnsetTimePlot = OnsetPoint/info.SampleRate;     
        plot(time, reading)
        xlim([min(time) max(time)])
        ylim([-1.5 1.5])
    hold on
        plot([OnsetTimePlot OnsetTimePlot], [-1.5 1.5], 'color', 'r', 'LineStyle', '-')
    title(aname,'interpreter','none')
    hold off
    
   
    m = input('Play the track?, 1/0/q:','s')  % interactively allows you to play the file and save data ...
  	if m == '1'
        sound(reading, info.SampleRate) 
        n = input('Enter if ONSET is VALID, 1/0:','s') % ... if the onset is valid...
        if n == '1'
            valid = 1;
        else
            valid = 0;
        end
        l = input('Enter if ANSWER is CORRECT, 1/0:','s') % ... and if the answer is correct
        if l == '1'
            correct = 1;
        else
            correct = 0;
        end
    elseif m == 'q'
        break
    end
   
    close(fig)
     
     OnsetTime(i, 1:2) = [aname,...          % saves all this in a cell
                          num2cell(OnsetPoint/info.SampleRate/.001)];%,...
                          %hasOnset,...
                          %valid,...
                          %correct];     
     Durations(i,1:2)  = [info.Duration,...
                          info.TotalSamples];    
                      
     %badOnset(i,1) = valid ==0 ;
end

checkDurations    = tabulate(categorical(Durations(:,2))); % check durations are equal. Notice that practice trials are longer

% for a = 1:sum(~badOnset)
%     retrieve = OnsetTime(~badOnset,1);
%     analyseAgain{a,1} = [retrieve{a}, '.wav']; 
%    % [status, result] = system(char(['C:\Users\pplsuser\Documents\software\Praat.exe ' analyseAgain(a,1)]).')
% 
% end


% fileID = fopen('CheckVocal_AudioFiles-datalist.txt');
% C = textscan(fileID,'%s %f');
% fclose(fileID);
% 
% compare = table((C{1,1}(:)),(C{1,2}(:)));
% 
% [R,P] = corrcoef(cell2mat(OnsetTime(:,2)), table2array(compare(:,2)));


