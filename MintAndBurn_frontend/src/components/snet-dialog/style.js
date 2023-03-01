import ColorCodes from '../../assets/theme/colorCodes';

const snetDialogStyles = {
  dialogTitle: {
    m: 0,
    color: ColorCodes.blue,
    fontSize: 18,
    fontWeight: '600',
    lineHeight: '24px',
    padding: '16px 30px !important',
  },
  iconButton: {
    position: 'absolute',
    right: 8,
    top: 8,
    color: (theme) => theme.palette.grey[700],
    '& svg': { fontSize: 24 }
  },
  dailogContent: {
    maxWidth: '800px',
    minWidth: '600px',
    padding: '16px 24px !important'    
  }
};

export default snetDialogStyles;
