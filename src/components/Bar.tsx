import React from 'react';
import {
  XYPlot,
  VerticalBarSeries,
  VerticalGridLines,
  HorizontalGridLines,
  XAxis,
  YAxis,
  ChartLabel
} from 'react-vis';
import { Box } from '@material-ui/core';
import data from '../data/data.json';

type Props = { param: string; label: string };

const getFreq = (data: number[], xMin: number, xMax: number) => {
  const segment = 8;
  const diff = (xMax - xMin) / segment;
  const thresholds = Array(segment)
    .fill(0)
    .map((_item, index) => xMin + ((xMax - xMin) * index + 0.5) / segment);
  return thresholds.map((item, index, array) => ({
    x: segment < 3 ? (item === 1 ? 'true' : 'false') : item,
    y: data.filter((item2: number) => {
      if (index < array.length - 1) {
        return item2 >= item && item2 < item + diff;
      } else {
        return item2 >= item;
      }
    }).length
  }));
};

type Record = {
  id: number;
  choice: number;
  lhfs1_el: number;
  lhfs1_train: number;
  lhfs1_auto: number;
  lhfs3_el: number;
  lhfs3_train: number;
  lhfs3_auto: number;
  minttime_el: number;
  minttime_train: number;
  minttime_auto: number;
};

const Bar: React.FC<Props> = (props: Props) => {
  const mergedData = (data as Record[])
    .reduce((acc: any[], current: Record) => {
      return [
        ...acc,
        {
          mode: 'el',
          value: current[(props.param + '_el') as keyof typeof current]
        },
        {
          mode: 'train',
          value: current[(props.param + '_train') as keyof typeof current]
        },
        {
          mode: 'auto',
          value: current[(props.param + '_auto') as keyof typeof current]
        }
      ];
    }, [] as Record[])
    .filter(item => item.value !== null);
  const xMin = Math.min(...mergedData.map(item => item.value));
  const xMax = Math.max(...mergedData.map(item => item.value));
  let freqE = getFreq(
    mergedData.filter(item => item.mode === 'el').map(item => item.value),
    xMin,
    xMax
  );
  let freqT = getFreq(
    mergedData.filter(item => item.mode === 'train').map(item => item.value),
    xMin,
    xMax
  );
  let freqA = getFreq(
    mergedData.filter(item => item.mode === 'auto').map(item => item.value),
    xMin,
    xMax
  );
  const freq = freqE.map(
    (item, index) => freqE[index].y + freqT[index].y + freqA[index].y
  );
  freqE = freqE.map((item, index) => ({
    x: item.x,
    y: freqE[index].y / freq[index]
  }));
  freqT = freqT.map((item, index) => ({
    x: item.x,
    y: freqT[index].y / freq[index]
  }));
  freqA = freqA.map((item, index) => ({
    x: item.x,
    y: freqA[index].y / freq[index]
  }));
  return (
    <Box
      margin={'0.6rem 0 0 0'}
      style={{ boxSizing: 'border-box', border: '1px gray solid' }}
    >
      <XYPlot
        colorType={'literal'}
        width={720}
        height={360}
        margin={{ left: 85, bottom: 65, right: 25 }}
        stackBy="y"
      >
        <VerticalGridLines />
        <HorizontalGridLines />
        <XAxis />
        <YAxis />
        <ChartLabel
          text={props.label}
          xPercent={0.5}
          yPercent={0.72}
          style={{
            textAnchor: 'middle'
          }}
        />
        <ChartLabel
          text={'# of Observations'}
          xPercent={0.04}
          yPercent={0.25}
          style={{
            transform: 'rotate(-90)',
            textAnchor: 'middle'
          }}
        />
        <VerticalBarSeries data={freqE} color={'#DAAA00'} opacity={0.8} />
        <VerticalBarSeries data={freqT} color={'#002855'} opacity={0.8} />
        <VerticalBarSeries data={freqA} color={'#9CAF88'} opacity={0.8} />
      </XYPlot>
    </Box>
  );
};

export default Bar;
