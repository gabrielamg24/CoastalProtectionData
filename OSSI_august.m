% OSSI Wave converter script
%
% Script to convert OSSI wave-logger data into an easily useable dataframe
% for investigating wave depth changes, wave conditions, and consequences 
% on bed shear stress
%
% Version 1.0 (12/06/2017)
% Coded and developed by Jim van Belzen (reorganized by Greg Fivash)
% published under the Creative Commons Attribution Non-Commercial license 
% which allows users to read, copy, distribute and make derivative works 
% for noncommercial purposes from the material, as long as the author of 
% the original work is cited.
% This code comes with no warranties or support
% http://people.zeelandnet.nl/jbelzen/

% Clear workspace & begin
clear all
clc

% Setup directories
mainDirectory = '/Users/gfivash/Desktop/OSSI Wave-logger SCRIPT';

% User Input: OSSI settings (& sediment data)
sampleFrequency = 5;           % sample frequency (Hz) [Usually 5 or 10 Hz]
burstInterval = 15/60; % burst interval: interval length (min)/60 minutes in an hour
installationHeight_m = 0.12;    % height of OSSI pressure sensitive disk above sediment surface (m)
activationDate = '2001-01-01 00:00:00'; % Date wave-logger was put in the field (yyyy-mm-dd hh:mm:ss)
D50 = 100 * 10^-6; % Sediment grain size dist. (D50)

airPressureFilename = 'KNMI_20170621_hourly.csv';
airPressureRecordingInterval = 60/60; % aire pressure record interval: interval length (min)/60 minutes in an hour
% Hacen air pressure: 1012

% Automated directories
inputDirectory = strcat(mainDirectory,'/Input');
outputDirectory = strcat(mainDirectory,'/Output');
functionDirectory = strcat(mainDirectory,'/Scripts/Functions');

%% Reformat raw data files in the /Raw folder
% Processed datafiles will be placed in the /Input folder
cd(inputDirectory)

fileNames = dir('*WLOG_*.CSV');
fileNumber = length(fileNames);

data = [];
temp = [];
A0 = [];

j = 1;
for i = 1:(fileNumber),
    fileName = fileNames(i).name;
    fidr = fopen(fileName, 'rt');
    i = 1; ii = 1;
    
    while feof(fidr) == 0,
        tline = fgetl(fidr);
        A = sscanf(tline, '%f');
        if isempty(A) == 0,
            a = textscan(tline,'%f','delimiter',',');
            D = a{1,1};
            if numel(D) == 13,
                data(i:(i + 11),j) = D(1:12);
                temp(ii,j) = D(13);
                i = i + numel(D) - 1;
                ii = ii + 1;
            end
        end
        if (isempty(A0) == 0) && (isempty(A) == 1),
            j = j + 1;
            i = 1; ii = 1;
        end
        A0 = A;
    end
    fclose(fidr);
end
% Write pressure & temperature datafiles
cd(inputDirectory)
csvwrite('bursts.csv',data)
csvwrite('temp.csv',temp)

%% Load OSSI & air pressure data
cd(inputDirectory)

%OSSI
pressureOSSIBAR = csvread(strcat(inputDirectory,'/bursts.csv')); %BAR units
pressureOSSIBAR(isnan(pressureOSSIBAR(:,1)),:) = [];%Remova NA values
pressureOSSIPa = 100000 * (0.97600 + pressureOSSIBAR); %Convert BAR to Pascal

%Air
dataPressureAirhPa = csvread(strcat(inputDirectory,'/',airPressureFilename)); %0.1 hPa (?) units
dataPressureAirhPa(isnan(dataPressureAirhPa(:,1)),:) = []; %Remova NA values
pressureAirPa = dataPressureAirhPa(:,5) * 10; %Retreive pressure data column and convert (0.1 hPa -> Pa)

%% Parameters
g = 9.8;          % gravitational acceleration (m/s^2)
rho = 1024;       % water density (kg/m^3)
win = 256;        % Window size
[n_meaurements,n_bursts] = size(pressureOSSIPa);  % number of bursts that wave loggers were in place
burst = [1:n_bursts];
time_days = [0:burstInterval:burstInterval*(n_bursts-1)]/24;
time_date = datetime(activationDate) + days(time_days);

%% Extract air pressure from weather station data and resample to fit burst data

% Alter pressure data to match bursts
% rep mat copies the one vector into a number of vector lines in an array
[x, y] = size(pressureOSSIPa);
% repat(data, x, y)  burst_interval/air_interval
pressureAirPa = repmat(pressureAirPa,1,airPressureRecordingInterval/burstInterval);
% reshape puts each of the lines into a vector
% reshape(rotated data,row_number, column number (= total cells in the array)  
pressureAirPa = reshape(pressureAirPa',1,numel(pressureAirPa));
pressureAirPa = pressureAirPa(1:y);
pressureAirPa = repmat(pressureAirPa,n_meaurements,1);

%%
% Convert pressure (Pascal) to water depth (m)
pressureWaterPa = pressureOSSIPa - pressureAirPa - min(mean(pressureOSSIPa - pressureAirPa));
waterHeightm = pressureWaterPa/(g*rho) + installationHeight_m;

% Excluding emerged moments when sensor does not function
submerged = mean(waterHeightm) > (0.015 + installationHeight_m);
h = (waterHeightm(:,submerged == 1));
dateSubmerged = datestr(time_date(:,submerged == 1), 'yyyy-mm-dd HH:MM:SS');
burstSubmerged = burst(submerged == 1);

meanPressureAirPa = mean((pressureAirPa(:,submerged == 1)));
meanPressureOSSIPa = mean((pressureOSSIPa(:,submerged == 1)));
meanPressureWaterPa = mean((pressureWaterPa(:,submerged == 1)));

%%
% Burst data cruncher
cd(functionDirectory)

[row, col] = size(h);
waves = [];


for i = 1:col,
    h_mean = mean(h(:,i));
    h_min = min(h(:,i));
    h_max = max(h(:,i));
    [amplitude, frequency] = WaveSpectrum(h(:,i), sampleFrequency, win);
    h_corr = AttenCorr(frequency, h_mean, installationHeight_m);
    amplitude = amplitude .* h_corr;
    [signif_wave_height, root_mean_sqr_wave_height, peak_wave_period, total_energy] = WaveChar(frequency, amplitude);
    orbital_velocity = (signif_wave_height' ./ (2 * h_mean)) .* sqrt(g * h_mean);
    BSS = 0.5 * rho *  2.5 * D50 * orbital_velocity.^2;
    
    wavesOSSI(i).burst = burstSubmerged(i);
    wavesOSSI(i).date = dateSubmerged(i,:);
    wavesOSSI(i).meanPressureAir_Pa = meanPressureAirPa(i);
    wavesOSSI(i).meanPressureOSSI_Pa = meanPressureOSSIPa(i);
    wavesOSSI(i).meanPressureWater_Pa = meanPressureWaterPa(i);
    wavesOSSI(i).meanWaterHeight_m = h_mean;
    wavesOSSI(i).minWaterHeight_m = h_min;
    wavesOSSI(i).maxWaterHeight_m = h_max;
    wavesOSSI(i).signifWaveHeight_m = signif_wave_height;
    wavesOSSI(i).rootMeanSqrWaveHeight_m = root_mean_sqr_wave_height;
    wavesOSSI(i).peakWavePeriod_inv_sec = peak_wave_period;
    wavesOSSI(i).totalEnergy_J = total_energy;
    wavesOSSI(i).orbitalVelocity_m_sec = orbital_velocity;
    wavesOSSI(i).BSS_Pa = BSS;
end
d = struct2table(wavesOSSI);

cd(outputDirectory)
writetable(d,'waves_OSSI.csv');
%% Inundation frequency (%)
innundationFrequency_perc = (sum(submerged)/n_bursts)*100